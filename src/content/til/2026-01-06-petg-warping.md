---
createdAt: "2026-01-06T12:00"
title: "PETG Wrapping and VFAs"
tags: ["3D Printing"]
draft: false
---

I've been getting back into doing more 3D printing in order to build my 
robot arms (more on that in a future post). But, some of the larger prints
have started to show signs of warping like this 
[reddit post complains about](https://www.reddit.com/r/prusa3d/comments/197ialb/mk4_petg_warping_how_to_solve/).

One of my robot arm pieces needed reprinting and it takes almost the entire 
plate and the wrapping was particularly bad. This made attaching it to the
other pieces a real challenge. But, it got me wondering, why didn't I have
this problem before. I can't be certain, but I believe my previous prints
were before I had upgrade my printer from MK4 to MK4S which upgraded the
nozzle for higher flow and a new print cooling fan that cools much faster.
And, it turns out that cooling layer of filament can cause it to pull on
the lower layers. At least according to this [video](https://www.youtube.com/watch?v=kTZaVPEbGrY).

I didn't spend much time investigating because this was the only really bad
print I had a few weeks ago. But, last week, I got my new Core One+ and
spent 2 days putting it together. I promptly printed a [gridfinity](https://www.youtube.com/watch?v=ra_9zU-mnl8)
box using PETG on a textured plate. (And, if you don't know what gridfinity is, 
what that video. seriously, watch it.) For some reason, the box warped badly
and so I had to dive into why the hell PETG warps. 

Here is what I learned:
1. For some unknown reason, the Core One+ Prusament PETG profile has a first and 
   other layer bed temperature of 85 degrees. My MK4S was 85 for the first and
   90 for the other layers. This higher temperature after the first layer helps
   to keep that first layer from lifting. Why did they do this? I don't know.
   I'm especially confused considering how
   [many people complain about warping](https://www.reddit.com/r/prusa3d/search/?q=warping).
2. Another important filament setting is how many layers before the fan turns on.
   There is the *Disable fan for first X layers* and *Full fan speed at layer* settings.
   See https://help.prusa3d.com/article/cooling_127569#fan-settings. I had to increase
   these to prevent the fan from turning on too early.
3. Mouse ears can help hold the corners down. Orca Slicer has a cool brim type called
   [mouse ears](https://github.com/OrcaSlicer/OrcaSlicer/wiki/others_settings_brim#mouse-ears).
   Unfortunately, PrusaSlicer doesn't 🤷. This [video](https://youtu.be/kTZaVPEbGrY?t=210) show
   a trick for adding manual mouse ears.

All of these helped, but I did have one [print](https://www.printables.com/model/1348713-prusa-core-one-side-storage-20x5x2-magnets)
yesterday that still had serious warping. So, I now have two hypotheses:

1. HT nozzle and the new fan are to blame. I'm going to try a good old 0.4 nozzle next.
2. The open vent to keep the chamber temp close to 35 degrees is creating a draft. I don't know what do about that. 
