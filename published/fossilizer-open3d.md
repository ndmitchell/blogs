# Generating Open 3D Viewer Models

_Summary: It's not obvious how to generate suitable Open 3D Viewer models, but with the right tools it isn't hard._

The [Open 3D Viewer](https://code.google.com/p/open-3d-viewer/) project is very cool, as an example here is a [demo of a spinning cow](http://open-3d-viewer.googlecode.com/svn/trunk/web/index.html) rendered in the browser. For my wife's work I wanted to generate a 3D model of a fossil bedding plane. Effectively, given some x/y/z coordinates, produce a pretty rendering. It took a fair bit of guess work, so I wrote down the steps.

**Step 1: Generate an OBJ file**

Generate an [OBJ file](http://en.wikipedia.org/wiki/Wavefront_.obj_file). You probably want an [MTL file](http://en.wikipedia.org/wiki/Wavefront_.obj_file#Material_template_library) too, but it seems the 3D viewer only uses the `Kd` field. There are a few ways to get an OBJ file:

* There are many samples on the web, including [a snail](https://code.google.com/p/open-3d-viewer/source/browse/#svn%2Ftrunk%2Fassets%2Fsnail_anatomy) in the Open 3D Viewer repo.
* You can create OBJ files in a tool such as [Blender](http://www.blender.org/), but the Blender interface confused me a lot (I am definitely not their intended audience).
* You can generate an OBJ file using a Haskell script. I picked this method, and I'll write a blog about the script later, once I have some pretty pictures to show.

**Step 2: Get the tools**

There are some tools in the [WebGL Loader](https://code.google.com/p/webgl-loader/) project. Alas, that project says "for now I recommend r50 as the last stable revision". So now there are two tools to try, the latest and r50. I tried both. I had some limited success with r50 (it didn't seem to render properly, but it did run) while the latest revision segfaulted. Fortunately I found the tools in a [Google Groups post](https://groups.google.com/d/msg/open-3d-viewer-discuss/MqfP16EqFwg/cAd0DGNt_K0J), and have mirrored them [in my repo](https://github.com/ndmitchell/fossilizer/tree/master/bin) (with trivial tweaks to support Python 2.7).

**Step 3: Run `objcompress`**

You need to run:

    objcompress mymodel.obj mymodel.utf8 > mymodel.js

This will generate lots of `mymodel*.utf8` files and `mymodel.js`.

**Step 3: Run `part_grouping.py`**

You need to run:

    py part_grouping.py

(The file in the email is `part&grouping.py`, but I renamed my copy.) This script will interactively ask a really long list of questions. I generate the correct inputs into a file and pipe it in:

    py part_grouping.py < response.txt

This generates the files `groupings.txt` and `part_info.txt`.

**Step 4: Run `make_viewer_metadata.py`**

Run:

    py make_viewer_metadata.py

This generates the file `entity_metadata.json`.

**Step 5: Get the viewer source**

You can get the viewer source from the [Open 3D Viewer repo](https://code.google.com/p/open-3d-viewer/). I have mirrored it [in my repo](https://github.com/ndmitchell/fossilizer/tree/master/web), but I may tweak the viewer over time to match my wife's needs - you should get the original.

**Step 6: Copy your files**

Copy all the files from steps 1 to 4 to a directory inside the viewer named `models/mymodel`.

**Step 7: Update the model list**

Open up `scripts/models.js` and edit it to point at your model. For example:

    o3v.MODELS = [{
      name:'mymodel.obj',
      scriptName:'mymodel.js',
      modelPath:'models/mymodel/',
      metadataFile:'entity_metadata.json',
      numLayers:12
    }];

**Step 8: View the result**

You can view the result by opening `index.html`. In Chrome you may need to pass the flag `--allow-file-access-from-files`.
