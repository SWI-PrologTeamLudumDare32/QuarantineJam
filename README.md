# QuarantineJam
The Team SWI-Prolog entry for the Quarantine Jam

## Theme

A LOT OF time to simulate farming!

## Team

* Gilbert B Garza
* Anne Ogborn

## Goals

 * Promote SWI-Prolog
 * Learn more Prolog
 * Have a complete, playable game

## Ideas

- A cow's pregnancy lasts about 279 days
- Herds get infected
- Cougar gets into chickens
- Cougar trap
- Offgrid power - windmill, water turbine
- Truck, tractor, fork lift, portable pump, wine press break down
- Pests - locusts, mites, weevils
- Prices of milk/meat/etc rise & fall, marketing
- Upgrade barn
- Weeding, fertilizing (have to buy fertilizer, it's a major expense)
- Fuel
- Weather - hail, blizzard, lack of rain, too much wind
- Somebody left the gate open, animals wander, some onto road and hit
- Farm machinery is dangerous. People die in grain bins
- Most such folks hunt to add meat to the family table
- Provide yur own power, sewer, water, phone, internet
- Plow, harrow, plant, water, fertilize, control weeds, harvest
- If it's a winery, the process has just started - press, blend, barrel age, bottle, sell
- Crops have to be rotated
- Do I store my grain and wait for price to rise or sell it? how much storage do I have?
- barn - a home for horses, cows, pigs, chickens.  A cow/horse barn has animals on the ground floor. 2nd floor is stacked with hay bales.
- Horses - need horses
- Donkeys, oxen, horses are the old way to get power to plow.
- Combine - cuts wheat or corn, and for wheat separates the chaff from the wheat kernel (threshing), stores wheat in bin, farm couple, the guy drives the combine
- The wife drives the farm truck to the elevator, sells the wheat, comes back and gets another load.
- Plow - turn the earth over so it's ready to be planted
- Planter - pulled trainer gizmo that puts the seeds in the ground
- Weed sprayer - sprays chemicals on the crop

## Game Design

You play Annette, the no nonsense wife of a couple who own a winery.

Your friend Priscilla and her husband have just purchased land they
intend to make into a winery, but don't know the
first thing about running it. Typical of them. LOL - first thing Priscilla did was
have stationary for the 'winery' printed!

Priscilla sends you letters asking for advice, often including pictures.

The top portion of the game shows Priscilla's desk, with the letter she's just written
to Annette, and the bottom shows Annette's typewriter. (Really indicative of their
personalities - Priscilla's in some fantasy 'winery owner' space with hand written notes,
and Annette makes short, businesslike replies. 

There are 3 buttons. Two are arrows that cycle through the possible responses from Annette.
One is a 'send this' button.  They're wired to the right and left arrows and the enter key.

When the player clicks 'send this', they get a new letter from Priscilla - and a little time
has passed (varies with what Priscilla did). Things like 'buy a cow' should take only a day
or two so if you don't get something reasonable, play isn't stopped. One option is always
a non-responsive letter with no advice. This just makes time pass on the server.

The server figures out what actions they can do with the things they currently
own. We randomly select a maximum number of these. These become an array of JSON
objects, with the name of the action, the add/remove, etc, and the text of Annette's letter.

It also lists the inventory, in two sections, one for things and one for assets.

It shows Priscilla's next letter.

And it gives the URL for the new image.

## Known Tasks

### Front end

- The SVG isn't rendering in Firefox. Probably related to https://bugzilla.mozilla.org/show_bug.cgi?id=995813
- make the surrounding HTML load the fonts. I believe I've found all the fonts, lmromandunh10-regular is Latin Modern Roman Dunhill. There's a useful tool in Inkscape, Extensions/type/Replace Font... List Fonts.
- The letter from priscilla's filter looks bad in Chrome
- js to populate all three text blocks
  - annettetext   - Annette's letter
  - notetext   - Priscilla's letter
  - inventtext  - the inventory
- Make the text flow properly in these boxes. It looks like SVG will do this for us - Inkscape makes a flowroot, then within that a flowregion and flowparagraph s. If not, we can do it on
the server side, but writi
- make the image load

### Back end

- Pick a small set of things they can do so w're sure we have a minimal 'done' game
- make time passing work with months and infrastructure to forbid planting in december
- write code to figure out what actions are possible now
- Make the action objects with annette's letters
- CHR support to make parts of Priscilla's letter
- assemble Priscillas letter
- compute the current inventory
- CHR support to figure out the new image
- make things go wrong (actual game play)

## Fonts we need

Found the following fonts:
'Karumbi, Normal'
'Latin Modern Roman Dunhill'
'Latin Modern Roman Dunhill, Normal'
'monospace, Normal'
Karumbi
monospace
sans-serif


