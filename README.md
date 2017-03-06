Random-Sample is a Lisp library for something which is not nearly as
simple as it sounds: reliably taking a random sample of a sequence.

Most of the time you will just use `random-sample`:

    (random-sample:random-sample (iota 100) 5)
    => (57 32 28 45 4)

The algorithm used for sampling without replacement is basically
Vitter’s [Algorithm D][Vitter].

Sometimes you may want to use the algorithm at a lower level. You
don’t want the sample itself; you only want the indices. In this case,
you can directly use `map-random-below`, which simply calls a provided
function on each index.

    (map-random-below my-function 5 length)

[Vitter]: http://www.ittc.ku.edu/~jsv/Papers/Vit87.RandomSampling.pdf
