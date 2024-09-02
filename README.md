# Claudius - A fantasy retro computer library.

Claudius started out trying to be a functional library like that of [TIC-80](https://tic80.com): A way to do some retro style graphics but in OCaml rather than in LUA. It still does that, but it now has grown slightly different API and feels like its own thing.

# Key concepts

Claudius mixes a bunch of concepts to create its fantasy console like environment, some of which is there to encourage functional approaches to

## The framebuffer

## Tick and Boot

# Screen modes

Claudius isn't as restrictive as a dedicated fantasy console, which typically offers one or a few dedicated modes (e.g., 240x180x16 for TIC-80), but rather you specify a screen as having a resolution and palette of your choosing. Currently palettes are only configurate at start-of-day, and not yet modifiable whilst an effect is running, but the ability to have palettes of arbitary sizes does offset this limitation somewhat.

## Palettes

You can create a 256 entry monochromatic palette like so:

```ocaml
open Claudius

let p = Palette.generate_mono_palette 256
```

Or a plasma colour palette like this:

```ocaml
open Claudius

let p = Palette.generate_plasma_palette 16
```

For other palettes, you can simply provide a list of 24bit colour values as integers:

```ocaml
open Claudius

let p = Palette.of_list [0x000000 ; 0xFF0000; 0x00FF00; 0x0000FF ; 0xFFFFFF]
```

## Screens

Once you have a colour palette defined, you can now create the screen mode you want:

```
open Claudius

let s = Screen.create 640 480 1 (Palette.generate_mono_palette 16)
```

The first two arguments are the width and height of the emulated screen mode, and final argument is the palette. The third argument is a scaling factor when displayed; if you're trying to work at resolutions like 320x200 (old-school VGA 256 colour), then things can get quite small on modern displays, so you might want to bump that up a bit, for example making it display at three times the size:

```
open Claudius

let s = Screen.create 320 200 3 (Palette.generate_plasma_palette 256)
```

## Define your
