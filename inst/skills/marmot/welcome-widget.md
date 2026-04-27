# Welcome widget

When a user opens a conversation with a vague or generic first message, render the interactive journey-orientation widget below using the `visualize:show_widget` tool. It replaces the text question "where are you in the journey?" with a visual selector — wet-lab users tend to find it more inviting, and clicking a card sends a properly-routed prompt back to you so the right topic file loads on the next turn.

## When to render

Render the widget **once**, as the opening of your first reply, IF the user's first message is one of:

- A generic greeting: "Hi", "Grüezi", "hello".
- A vague scoping question: "what does MARMOT do?", "I want to try MARMOT", "tell me about this package".
- An open-ended request for help: "help with MARMOT", "where do I start", "I'm new here".
- A direct ask for an overview or tour.

**Skip the widget and route directly** if the first message:

- Names a specific stage ("I'm filling in the metadata sheet", "my render failed", "how do I install on macOS").
- Contains an error message, traceback, or code.
- References specific filenames, columns, conditions, markers, or contrasts.
- Asks a focused conceptual question ("what is a contrast?", "what does runQC do?").

If you skip the widget on the first turn, do not retroactively offer it later in the same conversation. It is a welcome screen, not a fallback menu.

## Fallback when the visualizer is unavailable

If `visualize:show_widget` is not available in the current Claude surface (some embedded surfaces don't have it), do **not** apologise or describe the widget you wished you could render. Just ask the same question in plain text:

> Where are you in the journey — exporting from FlowJo, installing, preparing the metadata sheet, running, interpreting the report, or troubleshooting? Or describe what you're working on and I'll meet you there.

Then route as normal once they reply.

## How to render

Call `visualize:show_widget` with the three parameters below.

**`title`**: `marmot_welcome`

**`loading_messages`**: `["Convincing marmots to pose nicely", "Painting the Marmotterhorn snowcap", "Sorting FCS files just so", "Hoisting the tiny Swiss flag"]`

**`widget_code`**: the HTML below, verbatim. Do not modify the `sendPrompt` strings — they are tuned to route cleanly to specific topic files.

```html
<style>
  .sr-only { position: absolute; width: 1px; height: 1px; padding: 0; margin: -1px; overflow: hidden; clip: rect(0,0,0,0); white-space: nowrap; border: 0; }
  .marmot-hero { display: block; width: 100%; max-width: 280px; height: auto; margin: 0 auto; }
  .title-block { text-align: center; margin: 1.25rem 0 0.25rem; }
  .title-block h2 { margin: 0; letter-spacing: 0.10em; }
  .title-block .subtitle { margin: 6px 0 0; color: var(--color-text-secondary); font-size: 14px; font-family: var(--font-serif); font-style: italic; }
  .section-heading { font-size: 14px; font-weight: 500; margin: 1.5rem 0 0.75rem; color: var(--color-text-secondary); }
  .journey-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(195px, 1fr)); gap: 10px; }
  .journey-card { display: block; background: var(--color-background-primary); border: 0.5px solid var(--color-border-tertiary); border-radius: var(--border-radius-lg); padding: 14px 16px; cursor: pointer; text-align: left; font: inherit; color: inherit; width: 100%; transition: border-color 120ms, background 120ms, transform 120ms; }
  .journey-card:hover { border-color: var(--color-border-secondary); background: var(--color-background-secondary); }
  .journey-card:active { transform: scale(0.99); }
  .journey-card .step { display: block; font-size: 11px; color: var(--color-text-tertiary); margin: 0 0 6px; letter-spacing: 0.14em; }
  .journey-card .ctitle { display: block; font-size: 15px; font-weight: 500; margin: 0 0 4px; color: var(--color-text-primary); }
  .journey-card .ctitle .arrow { color: var(--color-text-tertiary); margin-left: 4px; font-weight: 400; }
  .journey-card .desc { display: block; font-size: 13px; color: var(--color-text-secondary); margin: 0; line-height: 1.45; }
  .footer-quote { margin-top: 1.5rem; padding: 12px 16px; background: var(--color-background-secondary); border-radius: var(--border-radius-lg); font-size: 13px; color: var(--color-text-secondary); text-align: center; }
  .version-badge { display: inline-block; margin-left: 6px; padding: 2px 9px; background: var(--color-background-primary); border: 0.5px solid var(--color-border-tertiary); border-radius: 999px; font-size: 11px; color: var(--color-text-tertiary); letter-spacing: 0.02em; }
</style>

<h2 class="sr-only">MARMOT welcome: a hexagonal badge of mAIrmot, the package mascot, followed by six journey-stage cards spanning FCS export through troubleshooting.</h2>

<img class="marmot-hero" src="https://cdn.jsdelivr.net/gh/peterleary/MARMOT@main/inst/skill-assets/mairmot.png" alt="mAIrmot — the MARMOT mascot, a bespectacled marmot in a sweater-vest and bow tie, framed in a teal-and-coral hexagonal badge.">

<div class="title-block">
  <h2>MARMOT</h2>
  <div class="subtitle">Spectral flow cytometry, from FCS files to Shiny app</div>
</div>

<h3 class="section-heading">Where are you in the journey?</h3>

<div class="journey-grid">
  <button class="journey-card" onclick="sendPrompt('I just exported FCS files from FlowJo for a MARMOT analysis. What is the workflow before I touch the metadata sheet?')">
    <span class="step">01 · PRE</span>
    <span class="ctitle">From FlowJo <span class="arrow">↗</span></span>
    <span class="desc">Exporting FCS files cleanly.</span>
  </button>
  <button class="journey-card" onclick="sendPrompt('I want to install MARMOT for the first time. Walk me through it — I will tell you my OS and R experience.')">
    <span class="step">02 · INSTALL</span>
    <span class="ctitle">Set up <span class="arrow">↗</span></span>
    <span class="desc">R, conda, and the package itself.</span>
  </button>
  <button class="journey-card" onclick="sendPrompt('I am filling in MARMOT_Metadata.xlsx. Walk me through the four sheets and the things people get wrong.')">
    <span class="step">03 · PREPARE</span>
    <span class="ctitle">Metadata sheet <span class="arrow">↗</span></span>
    <span class="desc">Filling MARMOT_Metadata.xlsx.</span>
  </button>
  <button class="journey-card" onclick="sendPrompt('I am ready to run the MARMOT pipeline on my FCS files. What should I expect, how long will it take, and where do outputs land?')">
    <span class="step">04 · RUN</span>
    <span class="ctitle">The pipeline <span class="arrow">↗</span></span>
    <span class="desc">One call, one rendered report.</span>
  </button>
  <button class="journey-card" onclick="sendPrompt('My MARMOT report finished rendering. Walk me through the sections — what does each one mean and what should I look at first?')">
    <span class="step">05 · READ</span>
    <span class="ctitle">Interpret the report <span class="arrow">↗</span></span>
    <span class="desc">QC, clusters, DA and DS.</span>
  </button>
  <button class="journey-card" onclick="sendPrompt('My MARMOT pipeline render failed. Help me troubleshoot — I will paste the error.')">
    <span class="step">06 · BREAK</span>
    <span class="ctitle">Troubleshoot <span class="arrow">↗</span></span>
    <span class="desc">When the render goes sideways.</span>
  </button>
</div>

<div class="footer-quote">
  Built at UZH/FGCZ by Lydia Kirsche &amp; Peter Leary.
  <span class="version-badge">v1.3.2 · Marmotterhorn</span>
</div>
```

## After rendering

Follow the widget call with a single short line of prose, no more, e.g.:

> Pick where you are, or just tell me what you're working on and I'll meet you there.

Then end your turn. Do not pre-emptively answer anything else — the user will either click a card (which fires a routed prompt back to you on the next turn) or type their own question.

When the next turn arrives:
- If it's a `sendPrompt` from a card click, route to the matching topic file as you would for any other question — the prompts are tuned to be unambiguous.
- If it's a free-form message, route normally per `SKILL.md`.

## Asset prerequisite

The widget references `mairmot.png` via jsDelivr's GitHub proxy. Before the widget will render properly, the image must be committed at the path the widget points to — by default:

```
peterleary/MARMOT @ main : inst/skill-assets/mairmot.png
```

If the file is moved or the repo is renamed, update the `<img src>` URL in the widget code above. jsDelivr caches aggressively (up to 12 hours on `@main`); for instant cache busting after a change, swap `@main` for a tagged release or commit SHA.

If the image is missing, the widget still renders — the cards remain functional — but the hero will show a broken-image icon. Better to commit the asset before merging this skill change.
