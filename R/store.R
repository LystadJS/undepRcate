# Each browser keeps the session row it last read. Conditional updates reject stale
# editor callbacks and double-clicks, including those from a second app process.
db_open <- function(path) {
  if (path != ":memory:") dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  ok <- FALSE
  on.exit(if (!ok) DBI::dbDisconnect(con))
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
  DBI::dbWithTransaction(con, {
    version <- DBI::dbGetQuery(con, "PRAGMA user_version")[[1L]]
    if (version > 2L) stop("This progress database needs a newer version of undepRcate.")
    if (version == 0L) {
      DBI::dbExecute(con, "CREATE TABLE sessions (
        id INTEGER PRIMARY KEY, focus TEXT NOT NULL, mode TEXT NOT NULL,
        ordering TEXT NOT NULL, target INTEGER NOT NULL CHECK(target >= 0),
        position INTEGER NOT NULL DEFAULT 1 CHECK(position >= 1), template_id TEXT NOT NULL,
        seed INTEGER NOT NULL, level INTEGER NOT NULL CHECK(level BETWEEN 1 AND 3),
        draft TEXT NOT NULL DEFAULT '', hints INTEGER NOT NULL DEFAULT 0 CHECK(hints BETWEEN 0 AND 4),
        passed INTEGER NOT NULL DEFAULT 0 CHECK(passed IN (0,1)),
        revision INTEGER NOT NULL DEFAULT 1 CHECK(revision > 0),
        started_at REAL NOT NULL, item_started_at REAL NOT NULL, ended_at REAL,
        state TEXT NOT NULL DEFAULT 'active' CHECK(state IN ('active','finished','ended'))
      )")
      DBI::dbExecute(con, "CREATE UNIQUE INDEX one_active_session ON sessions(state) WHERE state='active'")
      DBI::dbExecute(con, "CREATE TABLE attempts (
        id INTEGER PRIMARY KEY, session_id INTEGER NOT NULL REFERENCES sessions(id),
        position INTEGER NOT NULL CHECK(position >= 1), template_id TEXT NOT NULL,
        seed INTEGER NOT NULL, level INTEGER NOT NULL CHECK(level BETWEEN 1 AND 3),
        code TEXT NOT NULL, hints INTEGER NOT NULL CHECK(hints BETWEEN 0 AND 4),
        status TEXT NOT NULL CHECK(status IN ('pass','incorrect','syntax_error','runtime_error','timeout','infrastructure_error')),
        seconds REAL NOT NULL CHECK(seconds >= 0), execution_seconds REAL NOT NULL CHECK(execution_seconds >= 0),
        attempted_at REAL NOT NULL, practice_date TEXT NOT NULL
      )")
      DBI::dbExecute(con, "CREATE INDEX attempt_skill_time ON attempts(template_id, attempted_at)")
      DBI::dbExecute(con, "CREATE TABLE settings (key TEXT PRIMARY KEY NOT NULL, value TEXT NOT NULL)")
      DBI::dbExecute(con, "PRAGMA user_version = 1")
    }
    if (version < 2L) {
      DBI::dbExecute(con, "CREATE INDEX mastery_session_items ON attempts(session_id,template_id,id)")
      DBI::dbExecute(con, "CREATE INDEX mastery_seed_history ON attempts(template_id,seed,id)
                           WHERE status!='infrastructure_error'")
      DBI::dbExecute(con, "PRAGMA user_version = 2")
    }
  })
  ok <- TRUE
  con
}

db_close <- function(con) DBI::dbDisconnect(con)

active_session <- function(con) {
  row <- DBI::dbGetQuery(con, "SELECT * FROM sessions WHERE state='active'")
  if (nrow(row)) row else NULL
}

start_session <- function(con, focus, mode, ordering, target, template_id, seed,
                          level = 1L, now = Sys.time()) {
  DBI::dbWithTransaction(con, {
    if (!is.null(active_session(con))) stop("Finish or end the current practice session first.")
    DBI::dbExecute(con, "INSERT INTO sessions
      (focus,mode,ordering,target,template_id,hints,seed,level,started_at,item_started_at)
      VALUES (?,?,?,?,?,?,?,?,?,?)", params = list(focus, mode, ordering, target, template_id, as.integer(mode == "Guided"),
                                                  seed, level, as.numeric(now), as.numeric(now)))
    active_session(con)
  })
}

update_active <- function(con, s, assignments, values) {
  sql <- paste("UPDATE sessions SET", assignments,
               ", revision=revision+1 WHERE id=? AND revision=? AND state='active'")
  changed <- DBI::dbExecute(con, sql, params = c(values, list(s$id, s$revision)))
  if (changed != 1L) stop("Session changed in another tab. Reload to use the latest version.")
}

save_draft <- function(con, s, draft, hints = s$hints) {
  if (s$passed == 1L) stop("This repetition has already passed.")
  DBI::dbWithTransaction(con, {
    update_active(con, s, "draft=?, hints=MAX(hints,?)", list(draft, hints))
    active_session(con)
  })
}

record_attempt <- function(con, s, code, hints, status, seconds, execution_seconds = 0,
                           now = Sys.time()) {
  if (s$passed == 1L) stop("This repetition has already passed.")
  hints <- max(s$hints, hints)
  practice_date <- format(as.POSIXct(now, origin = "1970-01-01"), "%Y-%m-%d", tz = "")
  DBI::dbWithTransaction(con, {
    update_active(con, s, "draft=?, hints=?, passed=?", list(code, hints, as.integer(status == "pass")))
    DBI::dbExecute(con, "INSERT INTO attempts
        (session_id,position,template_id,seed,level,code,hints,status,seconds,execution_seconds,attempted_at,practice_date)
        VALUES (?,?,?,?,?,?,?,?,?,?,?,?)",
      params = list(s$id, s$position, s$template_id, s$seed, s$level, code, hints, status,
                    seconds, execution_seconds, as.numeric(now), practice_date))
    active_session(con)
  })
}

advance_session <- function(con, s, template_id = s$template_id, seed = s$seed,
                            level = s$level, now = Sys.time()) {
  if (s$passed != 1L) stop("Pass this repetition before advancing.")
  DBI::dbWithTransaction(con, {
    if (s$target > 0L && s$position >= s$target) {
      update_active(con, s, "state='finished', ended_at=?", list(as.numeric(now)))
    } else {
      update_active(con, s,
        "position=position+1, template_id=?, seed=?, level=?, draft='', hints=?, passed=0, item_started_at=?",
        list(template_id, seed, level, as.integer(s$mode == "Guided"), as.numeric(now)))
    }
    active_session(con)
  })
}

end_session <- function(con, s, now = Sys.time()) {
  DBI::dbWithTransaction(con, {
    update_active(con, s, "state='ended', ended_at=?", list(as.numeric(now)))
    NULL
  })
}

attempt_history <- function(con, include_code = TRUE) {
  fields <- c("id", "session_id", "position", "template_id", "seed", "level",
              if (include_code) "code", "hints", "status", "seconds", "execution_seconds",
              "attempted_at", "practice_date")
  sql <- paste("SELECT", paste(paste0("attempts.", fields), collapse = ","),
               ", sessions.mode, sessions.focus FROM attempts JOIN sessions",
               "ON sessions.id=attempts.session_id ORDER BY attempted_at, attempts.id")
  DBI::dbGetQuery(con, sql)
}

recent_answer_code <- function(con, ids) {
  if (!length(ids)) return(character())
  placeholders <- paste(rep("?", length(ids)), collapse = ",")
  rows <- DBI::dbGetQuery(con, paste0("SELECT id, code FROM attempts WHERE id IN (", placeholders, ")"),
                          params = as.list(ids))
  rows$code[match(ids, rows$id)]
}

get_setting <- function(con, key, default = NULL) {
  row <- DBI::dbGetQuery(con, "SELECT value FROM settings WHERE key=?", params = list(key))
  if (nrow(row)) row$value[[1L]] else default
}

set_setting <- function(con, key, value) {
  DBI::dbExecute(con, "INSERT INTO settings(key,value) VALUES (?,?)
    ON CONFLICT(key) DO UPDATE SET value=excluded.value", params = list(key, as.character(value)))
  invisible(value)
}
