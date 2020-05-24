Sources for Göktuğ's Website
============================

This repository contains the static website setup for [my website][ws].
The sources for the pages live in a separate, private repository because
it includes unpublished drafts.

[ws]: https://www.gkayaalp.com/

The code here is licensed under [the 3-clause BSD License][license].  I
doubt you'd want to just grab and use this project, but feel free to
take whatever you like and adapt for your website or to use this one as
a starting point for your projects.

[license]: ./LICENSE

What you see is a simple static website built using [Hakyll], trying
to use as minimal styling as possible to give it some identity without
departing too much from an [essential web look][mofo].  Sticking to good
simple HTML and minimal styling, and avoiding unnecessary interaction
ensures that the pages are accessible to the differently abled and to
people lacking decent [internet access][netspds], that they [respect
readers' decisions][cascade], eat up less resources, and maintain a nice,
sustainable [BMI][webobesity] level.

[Hakyll]: https://jaspervdj.be/hakyll/
[cascade]: https://www.w3.org/TR/css3-cascade/#cascading-origins
[mofo]: http://motherfuckingwebsite.com/
[netspds]: https://en.wikipedia.org/wiki/List_of_countries_by_Internet_connection_speeds#Average_connection_speeds
[webobesity]: https://idlewords.com/talks/website_obesity.htm

Get started
===========

Prepare:

1) You’ll need a fairly recent verion of [GHC], which if your OS does
   not provide, can be obtained via [ghcup].

   - I’m running `ghc 8.8.3`.  `8.10` seems to be too new, causes
     depenency conflicts.

[GHC]: https://www.haskell.org/ghc/
[ghcup]: https://downloads.haskell.org/~ghcup/

Build and run:

1) Run `cabal new-build` to download the dependencies and build.

2) Optionally, use the Hakyll’s `watch` script for running a
   Python 3 `http.server` to serve the content on `localhost:8000`:
   ```
   cabal new-run www-build -- watch
   ```
   This will also recompile on change, but that’s not good for all
   cases, see below.


3) Run `cabal new-run www-build -- build` to build the website, and
   `cabal new-run www-build -- clean` to remove Hakyll build
   artefacts.  This is especially necessary when SCSS partials are
   edited because Hakyll won’t recognise them and invalidate targets
   of SCSS files that depend on them.

Relevant documentation
======================

- [Hakyll tutorials][tutidx]
- [What's where in the Hakyll module
  tree?][where]
- [Github Pages & Hakyll][ghpages]
- [Sass] docs (I use SCSS variant)

[where]: https://jaspervdj.be/hakyll/tutorials/a-guide-to-the-hakyll-module-zoo.html
[ghpages]: https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html
[tutidx]: https://jaspervdj.be/hakyll/tutorials.html
[Sass]: https://sass-lang.com/documentation
