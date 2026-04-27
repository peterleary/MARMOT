# Cofactors

How MARMOT transforms raw fluorescence values, and how the user should tune the transformation.

## What a cofactor is

Spectral / flow cytometry data spans many orders of magnitude on a linear scale. To make it usable for clustering and visualisation, MARMOT applies an **arcsinh** transformation to every channel:

```
transformed = asinh(x / cofactor)
```

The **cofactor** controls how aggressively the transformation compresses the data. Smaller cofactor → less compression at low values, more visible bimodality, but noise gets stretched. Larger cofactor → smoother distributions, less noise, but you may flatten real signal into the negative pile.

In MARMOT, cofactors are set in the `Study Data` sheet under **`Cofactors for markers to use`** — one numeric value per marker, positionally aligned with `Markers to include`.

## The MARMOT default and why we are not endorsing it

The shipped template uses **2500 for every marker** (typical of a spectral dataset). This is a starting point, **not a recommendation**. The wiki is explicit that the marmots do not endorse a single-cofactor-for-everything approach and encourage users to tune per marker — for example by exploring different cofactors in Cytobank before settling on values, then transcribing them into the metadata sheet.

## How to tune: the cofactor histogram report

Every rendered MARMOT HTML report includes a **cofactor histograms** section (one panel per marker). For each marker the report shows the transformed-count distribution. Read them like this:

| What you see | What it usually means |
|---|---|
| **Bimodal distribution centred around zero** (one peak just below 0, one peak above) | Cofactor is well-tuned for that marker. This is the goal for most lineage / activation markers. |
| **Three or more peaks** | You probably **under-transformed** — cofactor too small. Real positive cells got split into multiple peaks instead of pooling into a clean positive shoulder. Try a larger cofactor. |
| **One peak only** (everything piled near zero, sometimes with a faint tail to the right) | You probably **over-transformed** — cofactor too large. The positive-cell signal got squashed into the negative blob. Try a smaller cofactor. But: a marker that is genuinely off in your sample (e.g. a marker for a lineage that isn't there) will also look like one peak — biology vs. transformation is the user's call. |
| **One peak near zero with a small but distinct positive tail** | Could be correctly transformed for a rare-population marker (e.g. some intracellular cytokines). Don't reflexively re-tune. |

## The workflow we recommend

1. Run MARMOT once with the template's default cofactor of 2500 across the board.
2. Open the cofactor histograms in the rendered HTML report.
3. For markers that look under- or over-transformed, **adjust just those markers' cofactors** in the `Study Data` sheet and re-run. The pipeline supports a reload path that skips re-importing FCS — see `running-the-pipeline.md` → *Re-running*.
4. Iterate until each marker's distribution looks reasonable.

This is unavoidably interpretive — there's no algorithm for "the right cofactor" because the right answer depends on signal strength, biology, and how the cytometer was set up.

## Alternative transformations (not currently in MARMOT)

The wiki notes the team is **interested in adding biexponential transformation** as an alternative to arcsinh and would welcome user input. If a user asks specifically about biexponential, point them at the Marmot author rather than improvising — it's a feature request, not a configuration.

## How to behave when a user asks about cofactors

- If they ask "what cofactor should I use?", do not give a number. Explain the histogram-tuning workflow and point them at the cofactor histograms section of their report.
- If they show you a histogram and ask "is this right?", apply the table above. State what the shape suggests and let them decide.
- Don't change cofactors in their metadata file for them. Tell them which marker, which row of `Cofactors for markers to use`, and what direction to move (up / down).
- If they ask about biexponential, treat it as a feature request, not a configuration, and direct them to the wiki / author.
