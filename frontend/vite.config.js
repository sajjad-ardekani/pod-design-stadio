import { defineConfig } from "vite";
import { configDefaults } from "vitest/config";
import { resolve } from "path";

export default defineConfig({
  test: {
    exclude: [...configDefaults.exclude, "target/**", "resources/**"],
    environment: "jsdom",
  },
  resolve: {
    alias: {
      "@target": resolve(__dirname, "./target/storybook"),
      "@penpot/plugins-runtime": resolve(
        __dirname,
        "vendor/penpot-plugins/libs/plugins-runtime"
      ),
    },
  },
  optimizeDeps: {
    // ensure Vite pre-bundles it in dev
    include: ["@penpot/plugins-runtime"],
  },
});
