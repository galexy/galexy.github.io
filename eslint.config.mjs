import js from '@eslint/js';
import tseslint from 'typescript-eslint';
import eslintPluginAstro from 'eslint-plugin-astro';
import eslintConfigPrettier from 'eslint-config-prettier';

export default [
  {
    ignores: ['dist/', '.astro/', 'node_modules/'],
  },
  js.configs.recommended,
  ...tseslint.configs.recommended,
  ...eslintPluginAstro.configs.recommended,
  eslintConfigPrettier,
  {
    files: ['src/scripts/**/*.js'],
    languageOptions: {
      globals: {
        document: 'readonly',
        navigator: 'readonly',
        setTimeout: 'readonly',
        console: 'readonly',
      },
    },
  },
  {
    files: ['src/plugins/**/*.mjs'],
    languageOptions: {
      globals: {
        URL: 'readonly',
      },
    },
  },
];
