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
   - GHC 8.8.3 and 8.6.5 work okay.
   - Cabal 3.2.0.0 and 2.4.0.0 work okay.
     - This setup uses the new project style.

[GHC]: https://www.haskell.org/ghc/
[ghcup]: https://downloads.haskell.org/~ghcup/

Build and run:

1) Optionally run `cabal new-build` to download the dependencies and build.
   - This is not necessary as `make` recipes will build the executable
     when necessary.

2) Optionally, use the Hakyll’s `watch` command to build when files change and
   serve the website on `127.0.0.1:8000`:
   ```
   make watch
   ```
   This will also recompile on change, but sometimes you do need to just rerun.
   In that case, kill the above process, run `make clean`, and run `make watch`
   again.
   - Run as `DRAFTS=yes make watch` in order to build with draft pages
     and blog posts.


3) Run `make clean` to remove files.

4) For a deep clean, run `git clean -dfx`. Beware, this’ll remove any
   untracked files.

Relevant documentation
======================

- [Hakyll tutorials][tutidx]
- [What's where in the Hakyll module tree?][where]
- [Github Pages & Hakyll][ghpages]
- [Sass] docs (I use SCSS variant)
- [Pagination]

[where]: https://jaspervdj.be/hakyll/tutorials/a-guide-to-the-hakyll-module-zoo.html
[ghpages]: https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html
[tutidx]: https://jaspervdj.be/hakyll/tutorials.html
[Sass]: https://sass-lang.com/documentation
[Pagination]: https://jaspervdj.be/hakyll/reference/Hakyll-Web-Paginate.html
