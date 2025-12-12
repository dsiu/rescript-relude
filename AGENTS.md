# AGENTS.md - Relude ReScript v12

## Documentation References

**IMPORTANT**: When working with code in this repository, always refer to these official documentation sources:

### ReScript Language Reference
- **Official Doc**: https://rescript-lang.org/docs/manual/
- **llm-full.txt**: https://rescript-lang.org/llms/manual/llm-full.txt
- **Use for**:
  - ReScript syntax and language features
  - Standard library APIs
  - Type system details
  - External bindings and interop patterns
  - Best practices and idioms

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
