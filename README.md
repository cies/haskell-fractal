### Haskell example:
A simple fractal image renderer
===============================

I've always been intrigued by fractals, and decided to make an example program
in [Haskell](http://www.haskell.org) that renders a fractal.

The code is based on Michael Bradshaw's [Mandelbrot](http://mjb.io/+/haskell-mandelbrot)
blog post, tweaked it for brevity and educational purposes. Finally I sharpened
the contrast a bit.  To find a nice contrast enhancing curve I used the awesome
curve fitter of [zunzun.com](http://zunzun.com).

The idea is that it can show some of Haskell's features in a short program
with a wonderfully complex output. Hopefully it is more suitable for explaining
Haskell to newcomers then the fibonacci sequence and the quick sort.


## Output

A fairly large rendering (10000 x 14142) is commited to this repository in PNG
format. Have a [look](https://raw.githubusercontent.com/cies/haskell-fractal/master/w10000_i1200.png)
(17MB, sorry Github).


## PDF version

This code comes with a [PDF version](https://github.com/cies/haskell-fractal/blob/master/haskell-fractal.pdf?raw=true)
of the fractal rendering combined with the Haskell source code used to create it.


## Usage

Buildig and running:

    ghc -fforce-recomp -Wall -O2 fractal.hs && ./fractal

To make test renderings a `width` of `2000` should be enough, for a final rendering
the `width` may be increased to something like `10000`.

The [Gimp](http://gimp.org) was used to convert the PGM file into a PNG.

Finally [ShareLatex](http://sharelatex.com) chimed in to compile the PDF.
