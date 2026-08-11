import { expect, test } from "bun:test";
import { readFileSync } from "node:fs";
import { runInNewContext } from "node:vm";

const source = readFileSync(
  new URL("../../main/resources/web/index.js", import.meta.url), "utf8"
).replace(/^import .* from "#examples";\n/, "");

function page() {
  class Element {
    listeners = new Map();
    captures = new Set();
    style = {};
    clientWidth = 800;
    clientHeight = 600;

    addEventListener(name, listener) { this.listeners.set(name, listener); }
    add() {}
    getBoundingClientRect() {
      return {
        width: parseFloat(this.style.width ?? "480"),
        height: parseFloat(this.style.height ?? "360"),
      };
    }
    setPointerCapture(id) { this.captures.add(id); }
    hasPointerCapture(id) { return this.captures.has(id); }
    releasePointerCapture(id) {
      this.captures.delete(id);
      this.send("lostpointercapture", { pointerId: id });
    }
    send(name, properties = {}) {
      const event = {
        pointerId: 1, isPrimary: true, button: 0, clientX: 0, clientY: 0,
        preventDefault() { this.defaultPrevented = true; }, ...properties,
      };
      this.listeners.get(name)?.(event);
      return event;
    }
  }
  const elements = Object.fromEntries(
    ["example", "toggle", "restart", "stage", "sketch", "resize"]
      .map(id => [id, new Element()])
  );
  runInNewContext(source, {
    document: { getElementById: id => elements[id] },
    window: new Element(),
    BrowserExampleRunner: class {},
    listExamples: () => [],
  });
  return elements;
}

test("only the captured primary pointer resizes or ends a drag", () => {
  const { resize, sketch } = page();
  resize.send("pointerdown");
  resize.send("pointerdown", { pointerId: 2, isPrimary: false, clientX: 200 });
  resize.send("pointermove", { pointerId: 2, clientX: 300 });
  resize.send("pointerup", { pointerId: 2 });
  resize.send("lostpointercapture", { pointerId: 2 });
  expect(sketch.style.width).toBeUndefined();
  resize.send("pointermove", { clientX: 10, clientY: 20 });
  expect(sketch.style).toEqual({ width: "500px", height: "380px" });
  resize.send("pointerup");
  expect(resize.captures.size).toBe(0);
  resize.send("pointermove", { clientX: 100 });
  expect(sketch.style.width).toBe("500px");
});

test("cancellation and unexpected capture loss end only the matching drag", () => {
  for (const event of ["pointercancel", "lostpointercapture"]) {
    const { resize, sketch } = page();
    resize.send("pointerdown");
    resize.send(event, { pointerId: 2 });
    resize.send("pointermove", { clientX: 10 });
    expect(sketch.style.width).toBe("500px");
    resize.send(event);
    resize.send("pointermove", { clientX: 20 });
    expect(sketch.style.width).toBe("500px");
    resize.send("pointerdown", { pointerId: 3 });
    resize.send("pointermove", { pointerId: 3, clientX: 10 });
    expect(sketch.style.width).toBe("520px");
  }
});
