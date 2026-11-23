/* Browser events carry the repetition ID so a delayed draft cannot overwrite the next item. */
(function () {
  "use strict";
  let token = "";
  let passed = false;
  let timer;
  let synced;
  let tracing;
  const editor = () => window.ace.edit("code");
  const source = () => document.querySelector("input[name='source_mode']:checked")?.value || "In-app";
  const send = (name, value) => Shiny.setInputValue(name, value, {priority: "event"});
  function buttons() {
    document.getElementById("check_button").disabled = !token || passed;
    document.getElementById("reset_button").disabled = !token || passed || source() !== "In-app";
    document.getElementById("advance").disabled = !token || !passed;
    document.getElementById("hint").disabled = !token || passed || !!document.querySelector("#hint_text [data-last-hint]");
  }
  function saveDraft() {
    if (token && !passed && source() === "In-app") send("draft_event", {token, code: editor().getValue()});
  }
  // The layout session measures soft wraps and tab stops. It is never bound to
  // an editor, Shiny input, clipboard or command. The real Ace document holds
  // only what the learner has written.
  function createTrace(aceEditor) {
    const renderer = aceEditor.renderer;
    const session = aceEditor.session;
    const EditSession = window.ace.require("ace/edit_session").EditSession;
    const layout = new EditSession("");
    layout.setUseWorker(false);
    const layer = document.createElement("div");
    layer.className = "trace-layer";
    layer.setAttribute("aria-hidden", "true");
    layer.hidden = true;
    renderer.content.insertBefore(layer, renderer.content.querySelector(".ace_text-layer"));
    let margin, vertical, horizontal, behaviours;
    let lines = [];
    let lastGuide = "";
    let dirty = true;
    let geometry = "";

    function scrollSpace(bottom, right, showV, showH) {
      if (renderer.scrollMargin.bottom !== bottom || renderer.scrollMargin.right !== right)
        renderer.setScrollMargin(margin.top, bottom, margin.left, right);
      if (renderer.getOption("vScrollBarAlwaysVisible") !== showV)
        renderer.setOption("vScrollBarAlwaysVisible", showV);
      if (renderer.getOption("hScrollBarAlwaysVisible") !== showH)
        renderer.setOption("hScrollBarAlwaysVisible", showH);
    }
    function draw() {
      if (!lines.length || !renderer.scroller.clientHeight) return;
      const wraps = session.getUseWrapMode();
      const width = session.getWrapLimit();
      const tabs = session.getTabSize();
      const wrapMethod = session.getOption("wrapMethod");
      const indent = session.getOption("indentedSoftWrap");
      const settings = [wraps, width, tabs, wrapMethod, indent].join("/");
      if (dirty || geometry !== settings) {
        layout.setTabSize(tabs);
        layout.setOption("wrapMethod", wrapMethod === "auto" ? (session.getMode().type === "text" ? "text" : "code") : wrapMethod);
        layout.setOption("indentedSoftWrap", indent);
        layout.setUseWrapMode(wraps);
        if (wraps) layout.adjustWrapLimit(width);
        layout.setValue(lines.map((guide, row) => {
          const typed = session.getLine(row);
          return typed + guide.slice(typed.length);
        }).join("\n"));
        dirty = false;
        geometry = settings;
      }

      const lineHeight = renderer.lineHeight;
      const charWidth = renderer.characterWidth;
      const scroller = renderer.scroller.getBoundingClientRect();
      const origin = layer.getBoundingClientRect();
      const left = scroller.left - origin.left + renderer.layerConfig.padding - renderer.scrollLeft;
      const top = scroller.top - origin.top - renderer.scrollTop;
      const firstVisible = Math.floor(renderer.scrollTop / lineHeight);
      const lastVisible = Math.ceil((renderer.scrollTop + scroller.height) / lineHeight);
      const docRows = session.getLength();
      const fragment = document.createDocumentFragment();
      let screenRows = session.getScreenLength();
      let maxColumns = 0;
      for (let row = 0; row < lines.length; row++) {
        const guide = lines[row];
        const used = session.getLine(row).length;
        const realRow = row < docRows;
        // Existing rows stay anchored to the learner's real document, including
        // independently wrapped alternatives. Future rows continue after it.
        const start = realRow ? session.documentToScreenRow(row, 0) : screenRows;
        const lastTypedRow = row === docRows - 1;
        const rowHeight = layout.getRowLength(row);
        if (!realRow || lastTypedRow) screenRows = Math.max(screenRows, start + rowHeight);
        if (used >= guide.length || start > lastVisible || start + rowHeight < firstVisible ||
            (realRow && session.getFoldLine(row))) continue;
        const nextReal = realRow && !lastTypedRow ? session.documentToScreenRow(row + 1, 0) : Infinity;
        const breaks = [...(layout.getRowSplitData(row) || []), guide.length];
        const shadowStart = layout.documentToScreenRow(row, 0);
        let from = used;
        for (const end of breaks) {
          if (end <= from) continue;
          const point = layout.documentToScreenPosition(row, from);
          const screenRow = start + point.row - shadowStart;
          if (screenRow >= nextReal || screenRow > lastVisible) break;
          if (screenRow >= firstVisible) {
            // Separate pieces at tabs: a CSS tab starting halfway through a
            // span otherwise uses that span's origin instead of Ace's tab stop.
            let column = from;
            while (column < end) {
              const tab = guide.indexOf("\t", column);
              const stop = tab < 0 || tab >= end ? end : tab + 1;
              const cell = layout.documentToScreenColumn(row, column);
              const span = document.createElement("span");
              span.className = "trace-fragment";
              span.dataset.row = row;
              span.dataset.column = column;
              span.style.left = (left + Math.round(cell * charWidth)) + "px";
              span.style.top = (top + screenRow * lineHeight) + "px";
              span.style.lineHeight = lineHeight + "px";
              span.textContent = guide.slice(column, stop);
              fragment.appendChild(span);
              column = stop;
            }
          }
          from = end;
        }
      }
      if (!wraps) maxColumns = layout.getScreenWidth();
      layer.replaceChildren(fragment);
      const extraY = Math.max(0, screenRows - session.getScreenLength()) * lineHeight;
      const extraX = Math.max(0, maxColumns - session.getScreenWidth()) * charWidth;
      scrollSpace(margin.bottom + Math.ceil(extraY), margin.right + Math.ceil(extraX),
        vertical || (extraY > 0 && screenRows * lineHeight > scroller.height),
        horizontal || (extraX > 0 && maxColumns * charWidth + 2 * renderer.layerConfig.padding > scroller.width));
    }
    session.on("change", () => { dirty = true; });
    renderer.on("afterRender", draw);
    return function setGuide(text) {
      const guide = text || "";
      if (guide === lastGuide) return;
      const active = guide.length > 0;
      if (active && !lastGuide) {
        margin = {...renderer.scrollMargin};
        vertical = renderer.getOption("vScrollBarAlwaysVisible");
        horizontal = renderer.getOption("hScrollBarAlwaysVisible");
        // An automatic closing delimiter would cover the next letter to type.
        behaviours = aceEditor.getBehavioursEnabled();
        aceEditor.setBehavioursEnabled(false);
      }
      lastGuide = guide;
      lines = active ? guide.split(/\r\n|\n|\r/) : [];
      dirty = true;
      layer.hidden = !active;
      const help = document.getElementById("typing-guide-help");
      const reference = document.getElementById("typing-guide-reference");
      if (help) help.hidden = !active;
      if (reference) reference.textContent = guide;
      const input = aceEditor.textInput.getElement();
      const ids = (input.getAttribute("aria-describedby") || "").split(/\s+/)
        .filter(id => id && id !== "typing-guide-note");
      if (active && help) ids.push("typing-guide-note");
      if (ids.length) input.setAttribute("aria-describedby", ids.join(" "));
      else input.removeAttribute("aria-describedby");
      if (!active) {
        layer.replaceChildren();
        layout.setValue("");
        scrollSpace(margin.bottom, margin.right, vertical, horizontal);
        aceEditor.setBehavioursEnabled(behaviours);
      }
      renderer.updateFull();
    };
  }
  window.undepRcate = {
    submit() {
      if (!token || passed) return;
      clearTimeout(timer);
      send("submission", {token, source: source(), code: editor().getValue()});
    },
    reset() {
      if (!token || passed || source() !== "In-app") return;
      editor().setValue("", -1);
      saveDraft();
      editor().focus();
    }
  };
  $(function () {
    const ordering = document.getElementById("ordering");
    let previousOrder = ordering.value;
    function updateOrder() {
      const guided = document.querySelector("input[name='mode']:checked")?.value === "Guided";
      if (guided === ordering.disabled) return;
      if (guided) previousOrder = ordering.value;
      ordering.value = guided ? "Sequential" : previousOrder;
      ordering.disabled = guided;
      $(ordering).trigger("change");
    }
    $(document).on("change", "input[name='mode']", updateOrder);
    updateOrder();
    $(document).on("click", "#finish_lesson", function () {
      send("finish_lesson", this.dataset.lessonToken);
    });
    $(document).on("click", "#retry_example", function () {
      send("retry_example", this.dataset.lessonToken);
    });
    Shiny.addCustomMessageHandler("rr-editor", function (message) {
      clearTimeout(timer);
      token = message.token;
      passed = message.passed;
      const ace = editor();
      ace.setReadOnly(!token || passed);
      ace.setValue(message.value, -1);
      ace.session.getUndoManager().reset();
      if (synced !== ace) {
        synced = ace;
        tracing = null;
        ace.session.on("change", () => {
          clearTimeout(timer);
          timer = setTimeout(saveDraft, 250);
        });
      }
      const guide = token && !passed ? message.guide : "";
      if (guide && !tracing) tracing = createTrace(ace);
      if (tracing) tracing(guide);
      buttons();
      if (token && !passed) setTimeout(() => { ace.resize(); if (source() === "In-app") ace.focus(); }, 75);
    });
    Shiny.addCustomMessageHandler("rr-passed", function (which) {
      if (token === which) {
        passed = true;
        editor().setReadOnly(true);
        if (tracing) tracing("");
        buttons();
      }
    });
    Shiny.addCustomMessageHandler("rr-accent", function (name) {
      document.documentElement.dataset.palette = name;
    });
    $(document).on("change", "input[name='source_mode']", () => {
      buttons();
      if (source() === "In-app" && tracing) requestAnimationFrame(() => editor().resize(true));
    });
    new MutationObserver(buttons).observe(document.getElementById("hint_text"), {childList:true, subtree:true});
    document.addEventListener("visibilitychange", () => {
      if (document.hidden && token && !passed) saveDraft();
    });
    document.addEventListener("keydown", e => {
      if (!token || !document.querySelector(".active-shell")?.offsetWidth ||
          document.querySelector(".modal.show")) return;
      if ((e.ctrlKey || e.metaKey) && e.key === "Enter") {
        e.preventDefault(); e.stopPropagation(); undepRcate.submit();
      } else if (e.altKey && e.key === "Enter") {
        e.preventDefault(); document.getElementById("advance").click();
      } else if (e.altKey && e.key === "Backspace") {
        e.preventDefault(); undepRcate.reset();
      }
    }, true);
    function tick() {
      document.querySelectorAll(".timer[data-start]").forEach(el => {
        if (el.dataset.stopped === "1") { el.textContent = "Saved"; return; }
        const elapsed = Math.max(0, Math.floor(Date.now()/1000 - Number(el.dataset.start)));
        const target = Number(el.dataset.target);
        const left = target ? target - elapsed : elapsed;
        const seconds = Math.abs(left);
        const clock = Math.floor(seconds/60) + ":" + String(seconds%60).padStart(2, "0");
        el.textContent = target ? (left < 0 ? "+" + clock + " over" : clock + " left") : clock + " elapsed";
        el.classList.toggle("over", target > 0 && left < 0);
      });
    }
    setInterval(tick, 350);
  });
})();
