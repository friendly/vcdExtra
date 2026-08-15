# Idea: Animation to illustrate pressure analog of area-based display of frequencies

In Friendly (1995), Conceptual and Visual Models for Categorical Data
https://www.datavis.ca/papers/amstat95.pdf
I discuss the physical analog of frequency data as molecules of gas in chambers,
where fitting a model by maximum likelihood corresponds to minimizing energy
and balancing forces.

These physical, and statistical properties are
shown visually as equal observation densities across the
cells in Figure 3 for the data on Hair color in the
`HairEyeColor` dataset.

This mechanical model also explains how we test
hypotheses about the true probabilities (Fig. 4). To test the
hypothesis that the four hair color categories are equally
probable,

$$
H_0: \pi_1 = \pi_2 = \pi_3 = \pi_4 = \frac14
$$

simply force the partitions to move to the hypothesized
values and measure how much energy is required to force
the constraint. Some of the chambers will then exert more
pressure, some less than when the forces are allowed to
balance without these additional restraints. The change in
energy in each compartment is then -(log pi - log 7ri) =
- log (pl/7r,), the change in negative log-likelihood. Sum
these up and multiply by 2 to get the likelihood ratio G2.
This gives a concrete instantiation of the interpretation of
G2 as a measure of the effort to maintain belief in the
hypothesis in the face of the data.

## Animation

* Set up a stack of rectangles whose heights are ~ frequencies,
$n_1, n_2, n_3, n_4$,
of the 4 hair colors

  * Randomly distribute $n_i$ points in each of the boxes.
  * Show these with distinct colors and shapes
  * To give a sense of pressure, and that the forces are balanced, let them move around by small random amounts over some set of frames
  * Begin to adjust the divisions between the chambers in the direction of making them all equal in height. Keep moving them around
    over some set of frames. One should then see the density of points change, reflecting the $G^2$, evidence against $H_0$

## Implementation notes (2026-08-14)

Not tied to vcdExtra for this — it's really a physics-style particle simulation
with an animated overlay, so the natural split is: (1) simulate the particle
motion + moving chamber walls, (2) render frames, (3) assemble into a GIF/video
or drive it live. Four viable routes:

### A. R + gganimate

Precompute one big long-format data frame: `frame, id, hair, x, y` for particles,
plus `frame, hair, ymin, ymax` for the moving partitions. Draw with
`geom_point()` + `geom_rect()`, `transition_manual(frame)`, render via
`gganimate::animate()` / `gifski`.

- + Fits the R/ggplot ecosystem — natural if this ends up in a vignette or
  paper reproduction alongside vcdExtra's other mosaic/fourfold plots.
  
- - Physics has to be fully precomputed before gganimate ever sees it (fine,
  just means the simulation loop is separate code, not "in" ggplot). Many
  points x many frames can render slowly.

### B. Base R + `animation::saveGIF()` (or `magick` + `gifski`)

Stateful for-loop: each iteration updates particle positions and partition
y-coords, draws with `rect()` + `points()`, one call per frame.

- + Simplest, most direct translation of the physics idea — no need to
  materialize a giant precomputed data frame, simulate and draw in the same
  loop. Fastest to prototype and iterate on the physics.
  
- - Less polished/shareable as a ggplot object; base-graphics look.

### C. Shiny app (reactive loop via `invalidateLater`)

Live simulation: chambers jiggle continuously; a button/slider triggers the
walls migrating toward the H0 (equal-height) configuration while particles
keep moving, so the density change is watched live rather than played back.

- + Best as a *teaching* tool — could ship as `vcdExtra::pressureDemo()` for
  talks/class. Natural place to add a slider interpolating between the ML
  fit and H0, or a live G² counter.
  
- - More build effort than a canned animation; not a static artifact you can
  drop in a paper or vignette without also exporting frames separately.

### D. Standalone HTML/JS (Canvas or SVG), no R involved

Client-side particle sim (random walk + reflect off walls) driven by
`requestAnimationFrame`; chamber walls are SVG rects with animated
y-position/height. Can add sliders for jiggle amplitude, phase durations,
particle count, speed.

- + Smoothest motion, cheapest to iterate on visually/interactively — good
  for nailing the "feel" (how much jiggle reads as pressure, how the
  compression should look) before committing to R code. Easy to share as a
  quick demo link.
  
- - Lives outside the package; would need porting to R (B or A) if the final
  deliverable must be an R-generated GIF for a vignette/paper.

### Recommended path

Prototype interactively in HTML (D) to tune the physics/visual parameters
quickly, then port the finalized version to R (B for a fast, simple loop, or
A/gganimate if a polished ggplot-native GIF is wanted for a vignette).

### Physics/algorithm sketch (applies to any of the above)

1. **Data**: `n <- margin.table(HairEyeColor, 1)` → Black, Brown, Red, Blond
   frequencies; `N <- sum(n)`.
   
2. **Chamber layout**: one vertical column, width `W`, total height `H`.
   Chamber heights `h_i = H * n_i / N`, stacked in category order — literally
   the area-proportional column from Fig. 3 (a 1-D spine plot).
   
3. **Particles**: seed `n_i` points per chamber at random `(x, y)` within
   that chamber's rectangle; color/shape by hair category.
   
4. **Phase 1 — equilibrium jiggle** (~T1 frames): each particle takes a
   small random step per frame, reflecting off its chamber's walls (including
   shared partitions) so it never leaves its chamber. Reads as "pressure
   balanced, no drift."
   
5. **Phase 2 — constraint / hypothesis test** (~T2 frames): linearly
   interpolate each partition's y-position from the data-based `h_i` toward
   the H0 value `H/4`. Each frame: (a) nudge partitions toward the target,
   (b) keep jiggling + reflecting particles off the *moving* walls. Chambers
   that shrink compress their particles (denser, more wall collisions — more
   "pressure"); chambers that grow spread out (less dense). This is a direct
   visualization of the unequal per-cell contribution to G².
   
6. **Optional overlay**: running/cumulative G² counter, or a per-chamber
   pressure indicator (color intensity, small gauge) tied to
   `-log(pi_hat_i / pi0_i)`, operationalizing the paper's energy argument.
   
7. **End state**: hold on the final frame — four equal-height, unequal-density
   chambers — with a caption giving the G² value for the Hair-color data.

Next step, if this direction looks right: build the HTML prototype (D) to
check the physics/feel, then decide between B and A for the R-side version.

### Prototype (2026-08-14)

Built: `issues/pressure-model-prototype.html` (published as the "Chamber
Pressure" artifact). Uses the real `HairEyeColor` hair margins (Black 108,
Brown 286, Red 71, Blond 127; N=592; G² = 165.59, df=3, p<.0001).

- Four stacked chambers sized by fitted proportions; 592 reflecting-random-walk
  particles (one per observation), colored by hair category.
  
- "Test H₀" button interpolates the partitions toward equal quarters
  (eased, adjustable duration) while particles keep jiggling against the
  moving walls — compresses Brown, vents Red, exactly as the pressure
  analogy predicts.
  
- Live "energy expended" readout = 2·Σ nᵢ·log(π̂ᵢ/πᵢ(t)), which is a
  monotonically increasing path from 0 to the true G² (it's N times the KL
  divergence from the fitted distribution to the partition's current
  distribution, convex along the linear interpolation path — guaranteed
  non-decreasing, so it doubles as a literal "effort so far" gauge).
  
- Sliders for jiggle amplitude ("molecular agitation") and transition speed;
  "Reset" lets the chambers relax back to the fitted state without
  re-seeding particles, so the re-expansion is watched live too.
  
- Respects `prefers-reduced-motion` (cuts jiggle amplitude to 0, shortens
  the transition) and both light/dark themes.

Take: the density-equalization effect reads clearly even at rest (Fig. 3's
point — equal density under the ML fit), and the compression/venting under
H₀ is visually obvious once triggered. Good enough to port. For the R
version, recommend base R + `gifski`/`animation::saveGIF` (route B) to
mirror this state machine most directly — the reflect-and-clamp particle
update and the eased partition interpolation translate almost line-for-line
into a per-frame R loop; save gganimate for a later, more polished pass if
the vignette wants a native-ggplot render.

