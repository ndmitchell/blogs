# Playing with Conduit

I've been a fan of lazy IO for a while, but Hoogle was turning into a space leaking disaster as a result. I took a stab at learning enumeartees at some point, but it was hard going, and looking at conduit a while back wasn't much fun either. The regular conduit and pipes tussle has resulted in a significant improvement.

I picked conduit because I use Wai/Warp, so conduit lets me write without going via lazy IO. I had a method in hoogle that converted Lazy.ByteString to conduit which broke in every release, and I kept emailing Michael. Eventually he added a feature directly to conduit.

My mental model for a conduit `Conduit a m b` is roughly a function `a -> m [b]` that you drive by keeping applying `a`'s and then you keep getting out `b`'s.

Taking an example from Hoogle, I had a list of files and contents (all produced using lazy IO from a zip file), and wanted to process N of them and concat them and wanted to compute two things. A set of the file names, and some transformation on their contents.

    files :: [(FilePath, String)]

    let body = concatMap (uncurry f) files
	let seen = Set.toList files 

Doing that in a way that runs in constant space, without accidentally space leaking, turns out to be very hard. Fortunately, in conduit it's pretty easy:

    files |$| (Set.toList <$> sinkList), concatMapC (uncurry f))

