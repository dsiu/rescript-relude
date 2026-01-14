# AGENTS.md - Relude ReScript v12

## Documentation References

**IMPORTANT**: When working with code in this repository, always refer to these official documentation sources:

### ReScript Language Reference
- **ALWAYS USE THIS FOR RESCRIPT CODE**: https://rescript-lang.org/llms/manual/llms.txt
- **LLM Full Documentation**: https://rescript-lang.org/llms/manual/llm-full.txt
- **Language Manual**: https://rescript-lang.org/docs/manual/introduction
- **Use for**:
  - ReScript syntax and language features
  - Standard library APIs
  - Type system details
  - External bindings and interop patterns
  - Best practices and idioms
- Ensure suggestions match this version. Refer to the indexed ReScript manual and LLM documentation.
- When dealing with promises, prefer using `async/await` syntax.
- Never ever use the `Belt` or `Js` modules, these are legacy.
- Always use the `JSON.t` type for json.
- Module with React components do require a signature file (`.resi`) for Vite HMR to work. Only the React components can be exposed from the javascript.

## Development Tools

### ReScript LSP Integration
- **IF the rescript-lsp plugin is installed and available in Claude Code**, use it to structurally analyze, search, and navigate ReScript code
- When available, prefer LSP features for:
  - Document symbols and structure analysis (viewing modules, types, functions)
  - Go to definition and find references
  - Type information and hover documentation
  - Structural navigation within functors and modules
- LSP-based code exploration is more accurate than text-based search for understanding ReScript module structure

## Build & Test Commands
- `yarn build` - Compile ReScript to JavaScript
- `yarn clean` - Clean build artifacts
- `yarn test` - Run all tests with Jest
- `yarn test -- --testPathPattern="Relude_Option"` - Run single test file
- `yarn test -- -t "isSome"` - Run tests matching a name pattern

## Code Style
- **Language**: ReScript v12 with ES modules (`.res` files, `.res.mjs` output)
- **Imports**: Use `open Jest` / `open Expect` in tests; prefer module aliases (`module Option = Relude.Option`)
- **Types**: Use ReScript native types; leverage type inference; use polymorphic variants for enums (e.g., `#equal_to`)
- **Naming**: camelCase for functions/values, PascalCase for modules/types; suffix test files with `_test.res`
- **Formatting**: 2-space indent; use pipe-first (`->`) for chaining; labeled args with `~name` syntax
- **Modules**: Use `include` to compose modules; organize by Base, Instances, Specializations pattern
- **Error Handling**: Use `Option`/`Result` types; avoid exceptions; `getOrThrow` only when safe
- **Tests**: Use `describe`/`test` blocks from `@glennsl/rescript-jest`; `testAll` for parameterized tests
- **Dependencies**: `rescript-bastet` for typeclass interfaces (Functor, Monad, etc.)

## Notes
- Ensure each test has only a single expect statement, using tuples where multiple results need to be tested
- Remember to use conventional commits spec for commit message
- Remember to run tests and make sure all tests passes before committing any changes
