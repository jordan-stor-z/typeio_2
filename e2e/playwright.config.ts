import { defineConfig, devices } from '@playwright/test';

// Playwright infrastructure for typeio's E2E suite -- see
// docs/solution-proposals/e2e-testing.md (#17) for the design rationale,
// e2e/README.md for how to run this locally.
//
// Deliberately does NOT start the app itself (no `webServer` entry):
// this suite drives a real running server + real seeded Postgres,
// started manually the same way local development already does
// (`make run-postgres`, `make migrate-up`, `make seed-db`,
// `cabal run server`) -- see README.md for the exact sequence.
//
// Not CI-wired yet (#98) -- no CI-specific retry/reporter config here
// for that reason, and trace/video capture is left at Playwright's
// defaults (off) until real CI runtime/artifact-storage cost is known.
export default defineConfig({
  testDir: './tests',
  fullyParallel: true,
  use: {
    baseURL: process.env.E2E_BASE_URL ?? 'http://localhost:3000',
  },
  // Single browser to start -- broaden only if a real cross-browser bug
  // surfaces.
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
});
