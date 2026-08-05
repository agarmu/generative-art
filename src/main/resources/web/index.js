import { BrowserExampleRunner, exampleNames } from "#examples";

const select = document.getElementById("example");
const toggle = document.getElementById("toggle");
const restart = document.getElementById("restart");
const stage = document.getElementById("stage");
const container = document.getElementById("sketch");
const resize = document.getElementById("resize");
const runner = new BrowserExampleRunner(container);
let drag = null;

for (const name of exampleNames()) {
  select.add(new Option(name, name));
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
  if (event.button !== 0) return;
  const { width, height } = container.getBoundingClientRect();
  drag = { x: event.clientX, y: event.clientY, width, height };
  resize.setPointerCapture(event.pointerId);
});

resize.addEventListener("pointermove", event => {
  if (!drag) return;
  const width = drag.width + 2 * (event.clientX - drag.x);
  const height = drag.height + event.clientY - drag.y;
  container.style.width = `${Math.max(100, Math.min(stage.clientWidth, width))}px`;
  container.style.height = `${Math.max(100, Math.min(stage.clientHeight, height))}px`;
});

resize.addEventListener("pointerup", event => {
  resize.releasePointerCapture(event.pointerId);
});

resize.addEventListener("lostpointercapture", () => { drag = null; });
