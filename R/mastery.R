# Within-session evidence for one drill. An old seed remains familiar across
# modes and difficulty changes; only its first graded encounter can earn credit.
mastery_checkpoint <- function(con, session_id, template_id) {
  attempts <- DBI::dbGetQuery(con, "
    SELECT a.status, a.hints, a.guide_shown,
           NOT EXISTS (SELECT 1 FROM attempts AS prior
                       WHERE prior.template_id=a.template_id AND prior.seed=a.seed
                         AND prior.id<a.id AND prior.status!='infrastructure_error') AS unseen
    FROM attempts AS a JOIN sessions AS s ON s.id=a.session_id
    WHERE s.mode='Mastery' AND a.session_id=? AND a.template_id=?
      AND a.status!='infrastructure_error'
    ORDER BY a.id", params = list(session_id, template_id))
  current <- 0L
  best <- 0L
  for (i in seq_len(nrow(attempts))) {
    if (attempts$status[i] != "pass" || attempts$hints[i] > 0L || attempts$guide_shown[i] == 1L) {
      current <- 0L
    } else if (attempts$unseen[i] == 1L) {
      current <- current + 1L
      best <- max(current, best)
    }
  }
  # A revealed hint ends the unaided run immediately, even before submission.
  pending <- DBI::dbGetQuery(con, "SELECT hints FROM sessions WHERE id=? AND template_id=?
                                  AND mode='Mastery' AND passed=0", params = list(session_id, template_id))
  if (nrow(pending) && pending$hints > 0L) current <- 0L
  list(current = current, best = best)
}
