---
title: FRP with Events and Behaviors - Part 1
location: BFPG
date: 2016-09-13
video-link: https://www.youtube.com/watch?v=GXW1jBijhlk
slides-link: https://github.com/dalaing/behaviors-and-events/raw/master/talk/bfpg-part1/slides.pdf
---

This is part of a series I'm doing focused on the event-and-behavior flavour of FRP.

There hasn't been a lot written about how to use FRP effectively - the FRP book by Blackheath and Jones is fantastic, but only came out recently.

The examples in that book are all in Java - which is great, because it means that you can deploy these tools in a lot of mainstream languages via the `sodium` library.
Still, I wanted to put something together using Haskell.

Part 1 focuses on the basics, and deals with the `reactive-banana` library throughout.
There's definitely more to say about the library, but hopefully it's enough to get people started.

Later parts will look at the evolution of a proper chat server written with FRP, and will focus on `reflex` and `reflex-dom`.
