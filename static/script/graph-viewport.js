// Scroll-and-zoom viewport for the server-rendered dependency graph
// (#179). No layout library: panning is the browser's own scrolling,
// and zoom is a scale factor written onto the SVG's width/height.
//
// The graph is a navigable viewport, not a fit-to-screen picture. It
// opens at a fixed readable scale with the project root in view, and a
// large project is expected to overflow and be scrolled -- shrinking it
// to fit would put its titles past legibility on exactly the projects
// that most need reading.
//
// This file is loaded by the graph fragment itself, so it re-runs on
// every htmx swap into #tree-container. Everything below is written to
// be idempotent under that: see `teardown` for how the previous run's
// listeners are dropped.
(() => {
  const container = document.getElementById("tree-container");
  const svg = document.getElementById("tree-view");
  if (!container || !svg) return;

  // The fragment is swapped in repeatedly, and #tree-container itself
  // survives each swap -- so listeners bound to it would accumulate one
  // set per swap. Each run aborts the previous run's signal, which
  // removes every listener it registered in one go.
  if (container._graphViewportTeardown) container._graphViewportTeardown();
  const ac = new AbortController();
  const signal = ac.signal;
  container._graphViewportTeardown = () => ac.abort();

  const num = (name, fallback) => {
    const v = parseFloat(svg.dataset[name]);
    return Number.isFinite(v) ? v : fallback;
  };

  // Natural size, from the layout engine's own bounds. Every zoom level
  // is a multiple of this rather than of whatever size the SVG happens
  // to have right now, so repeated zooming can't accumulate drift.
  const baseWidth = num("baseWidth", svg.clientWidth || 1);
  const baseHeight = num("baseHeight", svg.clientHeight || 1);

  const MIN_SCALE = 0.2;
  const MAX_SCALE = 3;
  const DEFAULT_SCALE = 1;
  const STEP = 1.2;

  let scale = DEFAULT_SCALE;

  const clamp = (v) => Math.min(MAX_SCALE, Math.max(MIN_SCALE, v));

  const applyScale = () => {
    svg.setAttribute("width", String(baseWidth * scale));
    svg.setAttribute("height", String(baseHeight * scale));
  };

  // Zoom about a fixed point: whatever diagram coordinate sits under
  // `clientX/clientY` stays under it afterwards. Without this, zooming
  // walks the graph off-screen -- the single thing that most makes a
  // hand-rolled zoom feel broken next to a mature one.
  const zoomAbout = (next, clientX, clientY) => {
    const target = clamp(next);
    if (target === scale) return;
    const rect = container.getBoundingClientRect();
    // Offset of the anchor point within the scrolled content, in
    // current-scale pixels...
    const ox = container.scrollLeft + (clientX - rect.left);
    const oy = container.scrollTop + (clientY - rect.top);
    const ratio = target / scale;
    scale = target;
    applyScale();
    // ...which moves by exactly `ratio` once the content is rescaled.
    container.scrollLeft = ox * ratio - (clientX - rect.left);
    container.scrollTop = oy * ratio - (clientY - rect.top);
  };

  // Zoom from a button: no pointer to anchor on, so hold the centre of
  // the visible area still instead.
  const zoomByStep = (factor) => {
    const rect = container.getBoundingClientRect();
    zoomAbout(scale * factor, rect.left + rect.width / 2, rect.top + rect.height / 2);
  };

  // The server already knows where the project root landed and emits it
  // as a data attribute, so the client never has to search the DOM for
  // it. Absent (a project with no root node), fall back to the middle
  // of the drawing.
  const centreOnRoot = () => {
    const rootX = num("rootX", baseWidth / 2);
    const rootY = num("rootY", baseHeight / 2);
    container.scrollLeft = rootX * scale - container.clientWidth / 2;
    container.scrollTop = rootY * scale - container.clientHeight / 2;
  };

  const recentre = () => {
    scale = DEFAULT_SCALE;
    applyScale();
    centreOnRoot();
  };

  // --- Pointer-drag panning ------------------------------------------
  //
  // Required because the scrollbars are hidden: without a scrollbar to
  // drag, a plain wheel-less mouse would have no way to pan at all.
  //
  // The hazard is that every node is also a click target (htmx opens
  // its detail panel), so a drag must not read as a click. A press only
  // becomes a pan past DRAG_THRESHOLD pixels, and once it does, the
  // click that the browser fires afterwards is swallowed exactly once.
  const DRAG_THRESHOLD = 4;
  let panPointer = null;
  let panStart = null;
  let dragged = false;

  // Live pointers, by id. One is a pan; two are a pinch.
  const pointers = new Map();
  let pinchStart = null;

  const pinchState = () => {
    const pts = [...pointers.values()];
    if (pts.length < 2) return null;
    const [a, b] = pts;
    return {
      dist: Math.hypot(a.x - b.x, a.y - b.y),
      cx: (a.x + b.x) / 2,
      cy: (a.y + b.y) / 2,
      scale,
    };
  };

  container.addEventListener(
    "pointerdown",
    (e) => {
      if (e.pointerType === "mouse" && e.button !== 0) return;
      pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
      if (pointers.size === 2) {
        // Second finger down: this is a pinch, not a pan.
        panPointer = null;
        pinchStart = pinchState();
        return;
      }
      if (pointers.size !== 1) return;
      panPointer = e.pointerId;
      dragged = false;
      panStart = {
        x: e.clientX,
        y: e.clientY,
        left: container.scrollLeft,
        top: container.scrollTop,
      };
    },
    { signal }
  );

  container.addEventListener(
    "pointermove",
    (e) => {
      if (!pointers.has(e.pointerId)) return;
      pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });

      if (pointers.size === 2 && pinchStart) {
        const now = pinchState();
        if (!now || !pinchStart.dist) return;
        zoomAbout(
          pinchStart.scale * (now.dist / pinchStart.dist),
          now.cx,
          now.cy
        );
        e.preventDefault();
        return;
      }

      if (e.pointerId !== panPointer || !panStart) return;
      const dx = e.clientX - panStart.x;
      const dy = e.clientY - panStart.y;
      if (!dragged && Math.hypot(dx, dy) < DRAG_THRESHOLD) return;
      if (!dragged) {
        dragged = true;
        container.classList.add("is-panning");
        // Take the pointer so the drag keeps tracking even when it
        // leaves the container -- otherwise a fast pan stalls at the
        // edge.
        try {
          container.setPointerCapture(e.pointerId);
        } catch (_) {
          /* not capturable (synthetic events in tests); pan still works */
        }
      }
      container.scrollLeft = panStart.left - dx;
      container.scrollTop = panStart.top - dy;
      e.preventDefault();
    },
    { signal }
  );

  const endPointer = (e) => {
    pointers.delete(e.pointerId);
    if (pointers.size < 2) pinchStart = null;
    if (e.pointerId === panPointer) {
      panPointer = null;
      panStart = null;
      container.classList.remove("is-panning");
      try {
        container.releasePointerCapture(e.pointerId);
      } catch (_) {
        /* never captured */
      }
    }
  };

  container.addEventListener("pointerup", endPointer, { signal });
  container.addEventListener("pointercancel", endPointer, { signal });

  // Capture phase, so the click is stopped before it reaches the node's
  // htmx handler. `dragged` is cleared here rather than on pointerup so
  // that exactly one click is swallowed per drag.
  container.addEventListener(
    "click",
    (e) => {
      if (!dragged) return;
      dragged = false;
      e.stopPropagation();
      e.preventDefault();
    },
    { capture: true, signal }
  );

  // --- Wheel zoom ----------------------------------------------------
  //
  // Only with ctrl/meta held. That is also what a trackpad pinch
  // reports as, so this one handler covers both. A plain wheel is left
  // alone deliberately: it's the container's native scroll, which is
  // the pan gesture.
  container.addEventListener(
    "wheel",
    (e) => {
      if (!e.ctrlKey && !e.metaKey) return;
      e.preventDefault();
      zoomAbout(scale * Math.exp(-e.deltaY * 0.002), e.clientX, e.clientY);
    },
    { passive: false, signal }
  );

  // --- Controls ------------------------------------------------------
  const on = (id, fn) => {
    const el = document.getElementById(id);
    if (el) el.addEventListener("click", fn, { signal });
  };
  on("graph-zoom-in", () => zoomByStep(STEP));
  on("graph-zoom-out", () => zoomByStep(1 / STEP));
  on("graph-zoom-reset", recentre);

  // --- Start ---------------------------------------------------------
  applyScale();
  // Defer the initial scroll one frame: the fragment has just been
  // swapped in, and clientWidth/clientHeight are only meaningful once
  // the browser has laid it out.
  requestAnimationFrame(centreOnRoot);
})();
