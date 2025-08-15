import * as esbuild from "esbuild";
import {readFile} from "node:fs/promises";
import path from "path";

const filter =
  /react-virtualized[/\\]dist[/\\]es[/\\]WindowScroller[/\\]utils[/\\]onScroll\.js$/;

const fixReactVirtualized = {
  name: "esbuild-plugin-react-virtualized",
  setup({onLoad}) {
    onLoad({filter}, async ({path}) => {
      const code = await readFile(path, "utf8");
      const broken = `import { bpfrpt_proptype_WindowScroller } from "../WindowScroller.js";`;
      return {contents: code.replace(broken, "")};
    });
  },
};

const redirectPluginsRuntime = {
  name: "penpot-plugins-runtime-alias",
  setup(build) {
    build.onResolve({filter: /^@penpot\/plugins-runtime$/}, () => ({
      // point at the TS source entry
      path: path.resolve(
        process.cwd(),
        "vendor/penpot-plugins/libs/plugins-runtime/src/index.ts"
      ),
    }));
  },
};

const rebuildNotify = {
  name: "rebuild-notify",
  setup(build) {
    build.onEnd((result) => {
      // console.log(result);
      // [:main] Build completed. (1003 files, 1 compiled, 0 warnings, 9.06s)
      console.log(
        `[:libs] Build completed. (${result.errors.length} warnings, ${result.errors.length} errors)`,
      );
    });
  },
};

const config = {
  entryPoints: ["target/index.js"],
  bundle: true,
  format: "iife",
  banner: {
    js: '"use strict";',
  },
  outfile: "resources/public/js/libs.js",
  loader: {
    ".svg": "dataurl",
    ".css": "text",
  },
  plugins: [fixReactVirtualized, redirectPluginsRuntime, rebuildNotify],
};

async function watch() {
  let ctx = await esbuild.context(config);
  return ctx.watch();
}

if (process.argv.includes("--watch")) {
  await watch();
} else {
  const localConfig = {...config, minify: true};
  await esbuild.build(localConfig);
}
