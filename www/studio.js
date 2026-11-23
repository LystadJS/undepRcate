/* Browser events carry the repetition ID so a delayed draft cannot overwrite the next item. */
(function () {
  "use strict";
  let token = "";
  let passed = false;
  let timer;
  let synced;
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
        ace.session.on("change", () => {
          clearTimeout(timer);
          timer = setTimeout(saveDraft, 250);
        });
      }
      buttons();
      if (token && !passed) setTimeout(() => { ace.resize(); if (source() === "In-app") ace.focus(); }, 75);
    });
    Shiny.addCustomMessageHandler("rr-passed", function (which) {
      if (token === which) {
        passed = true;
        editor().setReadOnly(true);
        buttons();
      }
    });
    Shiny.addCustomMessageHandler("rr-accent", function (name) {
      document.documentElement.dataset.palette = name;
    });
    $(document).on("change", "input[name='source_mode']", buttons);
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
