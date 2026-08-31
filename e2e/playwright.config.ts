import { defineConfig, devices } from '@playwright/test';

// Playwright infrastructure for typeio's E2E suite -- see
// docs/solution-proposals/e2e-testing.md (#17, decided §8) for why
// Playwright, and e2e/README.md for how to run this locally.
//
// Deliberately does NOT start the app itself (no `webServer` entry):
// per the decided proposal's §6/the #94 ticket, this suite drives a
// real running server + real seeded Postgres, started manually the same
// way local development already does (`make run-postgres`,
// `make migrate-up`, `make seed-db`, `cabal run server`) -- see
// README.md for the exact sequence. Auto-starting the app here would be
// new scaffolding the ticket deliberately avoided inventing.
//
// Not CI-wired yet (#98, a follow-up ticket) -- no CI-specific
// retry/reporter config here for that reason, and trace/video capture
// is deliberately left at Playwright's defaults (off) per the
// proposal's §7: worth deciding once real CI runtime/artifact-storage
// cost is known, not sight-unseen.
export default defineConfig({
  testDir: './tests',
  fullyParallel: true,
  use: {
    baseURL: process.env.E2E_BASE_URL ?? 'http://localhost:3000',
  },
  // Single browser (Chromium) to start, per the proposal's §7 "start
  // narrow" call -- broaden only if a real cross-browser bug surfaces.
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
});
