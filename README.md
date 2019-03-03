# Stackage-Everything generator

tl;dr: »I want Stackage on an airplane and I have only 3 minutes until takeoff«  
(If you actually want this to complete in less than 3 minutes you're probably out of luck.)

That being said, it's pretty neat.
It differs from quchen's original repo by being self contained rather than generating a script.
Might as well do the easy bit in Haskell too, eh?
Plus, it simplifies the running significantly.

This small script generates a script to download all the sources of a Stackage LTS release, so they can later be built/installed (including documentation) even when no internet connection is available.
A full download takes only a couple of minutes and around 60 megabytes of traffic at the time of writing this.
So if you’ve got a long flight ahead of you, or a weekend with your Granny in Siberia, or a dive into the Mariana Trench, or even worse – a trip through the German countryside by train – this is for you.

## Usage

```bash
stack build
stack exec -- stackage-everything-exe --lts 13.9
```

Afterwards, you’ll be able to compile packages via the usual means (`stack build` or as dependencies) without internet access, because Stack first searches the local folders for source files before attempting to download them.

## Addendum

There is a bash script, `stackage-everything.sh`, which does the same thing.
I wrote it up on the fly in about 5 minutes.
Page 10 of [this wonderful article](https://www.cs.tufts.edu/~nr/cs257/archive/don-knuth/pearls-2.pdf) comes to mind.
