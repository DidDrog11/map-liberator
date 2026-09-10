# 0007. Reference source is the original file, shown in the browser's own viewer

Date: 2026-09-10. Status: accepted.

## Context

Situation reports arrive as PDFs. The sidecar originally accepted PNG and
JPEG only, so a page had to be rendered to an image first. Rendering
needs R tooling on the operator's machine and is a step the intended
users may not have.

## Options

1. Keep images only; provide a script that renders report pages.
2. Render server-side on upload, adding a PDF dependency to the deployed
   app.
3. Accept the PDF and show it in the browser's built-in viewer.

## Decision

Option 3. A PDF upload is copied under a random per-session path
registered with Shiny and shown in an iframe. Images keep the existing
rotate, zoom and pan. The upload limit is raised to 50 MB.

## Why

- The intended user has a browser and a report, and nothing else. Every
  browser has a PDF viewer with paging, zoom and search.
- No new app dependency, no processing on the server, no conversion step
  in the protocol.
- The evaluation must reflect what users will do. Pre-rendered pages would
  make it easier than the real task.

## Consequences

- Provenance is the uploaded file name, which for the NCDC archive
  encodes year and epi week.
- Rendered pages produced by the archive script remain as optional
  convenience output and are not part of the protocol.
- Data-URI PDFs were rejected because browsers cap their size; the served
  path is the reason a per-session directory exists and is removed on
  session end.
