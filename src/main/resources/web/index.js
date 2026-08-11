import { BrowserExampleRunner, listExamples } from "#examples";

const select = document.getElementById("example");
const toggle = document.getElementById("toggle");
const restart = document.getElementById("restart");
const stage = document.getElementById("stage");
const container = document.getElementById("sketch");
const resize = document.getElementById("resize");
const runner = new BrowserExampleRunner(container);
let drag = null;

for (const { name, ident } of listExamples()) {
  select.add(new Option(name, ident));
}

function stop() {
  runner.endExample();
  toggle.textContent = "Run";
}

function run() {
  runner.startExample(select.value);
  toggle.textContent = runner.isRunning() ? "Stop" : "Run";
}

select.addEventListener("change", run);
toggle.addEventListener("click", () => runner.isRunning() ? stop() : run());
restart.addEventListener("click", run);
window.addEventListener("pagehide", stop, { once: true });

resize.addEventListener("pointerdown", event => {
  if (!event.isPrimary || event.button !== 0 || drag) return;
  const { width, height } = container.getBoundingClientRect();
  drag = { pointerId: event.pointerId, x: event.clientX, y: event.clientY, width, height };
  resize.setPointerCapture(event.pointerId);
});

resize.addEventListener("pointermove", event => {
  if (!drag || event.pointerId !== drag.pointerId) return;
  const width = drag.width + 2 * (event.clientX - drag.x);
  const height = drag.height + event.clientY - drag.y;
  container.style.width = `${Math.max(100, Math.min(stage.clientWidth, width))}px`;
  container.style.height = `${Math.max(100, Math.min(stage.clientHeight, height))}px`;
});

function endResize(event) {
  if (!drag || event.pointerId !== drag.pointerId) return;
  drag = null;
  if (resize.hasPointerCapture(event.pointerId)) {
    resize.releasePointerCapture(event.pointerId);
  }
}

resize.addEventListener("pointerup", endResize);
resize.addEventListener("pointercancel", endResize);
resize.addEventListener("lostpointercapture", endResize);
