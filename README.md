Haskell example: Simple fractal image renderer
==============================================

I've always been intrigued by fractals, and decided to make an example program
in [Haskell](http://www.haskell.org) that renders a fractal.

I based it on Michael Bradshaw's [Mandelbrot](http://mjb.io/+/haskell-mandelbrot)
code, tweaked it for brevity and sharpened the contrast a bit.

The idea is that it can show some of Haskell's features in a short program
with a wonderfully complex output. Hopefully it is more suitable for explaining
Haskell to newcomers then the fibonacci sequence and the quick sort.


## PDF version

This code comes with a PDF version of the fractal rendering combined with the
Haskell source code used to create it.


## Usage

Buildig and running:

    ghc --make fractal.hs && ./fractal

To make test renderings a `width` of `2000` should be enough, for a final rendering
the `width` may be increased to something like `10000`.

I used the [Gimp](http://gimp.org) to convert the PGM file into a PNG.

Finally I used [ShareLatex](http://sharelatex.com) to compile the PDF.
