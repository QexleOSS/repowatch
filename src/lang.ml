(* Language registry: extension -> language, and what we can prove per language. *)

let by_ext =
  [
    ("ts", "typescript"); ("mts", "typescript"); ("cts", "typescript");
    ("tsx", "tsx"); ("js", "javascript"); ("jsx", "javascript");
    ("mjs", "javascript"); ("cjs", "javascript");
    ("py", "python"); ("pyi", "python");
    ("rs", "rust"); ("go", "go");
    ("c", "c"); ("h", "c");
    ("cpp", "cpp"); ("cc", "cpp"); ("cxx", "cpp"); ("hpp", "cpp");
    ("hh", "cpp"); ("hxx", "cpp");
    ("cs", "c_sharp"); ("java", "java");
    ("rb", "ruby"); ("php", "php");
    ("ml", "ocaml"); ("mli", "ocaml");
    ("adb", "ada"); ("ads", "ada");
    ("zig", "zig"); ("hs", "haskell");
    ("lua", "lua"); ("sql", "sql");
    ("kt", "kotlin"); ("kts", "kotlin");
    ("scala", "scala"); ("sc", "scala");
    ("swift", "swift"); ("dart", "dart");
    ("ex", "elixir"); ("exs", "elixir");
    ("erl", "erlang"); ("hrl", "erlang");
    ("clj", "clojure"); ("cljs", "clojure"); ("cljc", "clojure");
    ("r", "r"); ("jl", "julia");
    ("m", "objc"); ("mm", "objc");
    ("sol", "solidity"); ("proto", "protobuf");
    ("vue", "vue"); ("svelte", "svelte");
    ("md", "markdown"); ("mdx", "markdown"); ("rst", "markdown"); ("txt", "markdown");
    ("pdf", "pdf");
    ("png", "image"); ("jpg", "image"); ("jpeg", "image");
    ("gif", "image"); ("webp", "image"); ("svg", "image");
    ("yaml", "yaml"); ("yml", "yaml"); ("json", "json"); ("toml", "toml");
    ("tf", "terraform");
    ("nix", "nix"); ("pl", "perl"); ("pm", "perl");
    ("fs", "fsharp"); ("fsi", "fsharp"); ("fsx", "fsharp");
    ("groovy", "groovy"); ("gradle", "groovy");
    ("ps1", "powershell"); ("psm1", "powershell");
    ("f90", "fortran"); ("f95", "fortran"); ("f03", "fortran"); ("f08", "fortran");
    ("pas", "pascal"); ("pp", "pascal");
    ("graphql", "graphql"); ("gql", "graphql");
    ("nim", "nim"); ("cr", "crystal"); ("d", "d");
    ("v", "verilog"); ("sv", "verilog"); ("vh", "verilog");
    ("vhd", "vhdl"); ("vhdl", "vhdl");
    ("cu", "cpp"); ("cuh", "cpp");
    ("bzl", "starlark"); ("cmake", "cmake");
    ("vb", "vb"); ("asm", "asm"); ("s", "asm");
    ("csproj", "msbuild"); ("fsproj", "msbuild"); ("vbproj", "msbuild");
    ("props", "msbuild"); ("targets", "msbuild"); ("sln", "sln");
    ("razor", "razor"); ("cshtml", "razor"); ("xaml", "xaml");
    ("pro", "prolog"); ("lisp", "lisp"); ("scm", "lisp"); ("rkt", "lisp"); ("el", "lisp");
    ("sh", "shell"); ("bash", "shell"); ("zsh", "shell");
    ("elm", "elm"); ("vala", "vala"); ("cob", "cobol"); ("cbl", "cobol"); ("cpy", "cobol");
    ("glsl", "glsl"); ("vert", "glsl"); ("frag", "glsl"); ("geom", "glsl");
    ("comp", "glsl"); ("tesc", "glsl"); ("tese", "glsl");
    ("hlsl", "hlsl"); ("fx", "hlsl"); ("fxh", "hlsl");
    ("cl", "opencl"); ("abap", "abap"); ("cls", "apex"); ("trigger", "apex");
  ]

let extension path =
  let ext = Filename.extension path in
  if String.length ext > 1 then String.sub ext 1 (String.length ext - 1) else ""

let of_path path =
  match Filename.basename path with
  | "Dockerfile" | "Containerfile" -> Some "dockerfile"
  | "Makefile" | "GNUmakefile" -> Some "makefile"
  | "CMakeLists.txt" -> Some "cmake"
  | "BUILD" | "WORKSPACE" | "BUILD.bazel" | "WORKSPACE.bazel" -> Some "starlark"
  | name ->
    (match List.assoc_opt (String.lowercase_ascii (extension name)) by_ext with
     | Some lang -> Some lang
     | None -> None)

type tier = Full | Imports | Docs | Files

let tier = function
  | "typescript" | "tsx" | "javascript" | "python" | "rust" | "go"
  | "c" | "cpp" | "java" | "c_sharp" | "ocaml" | "zig"
  | "kotlin" | "scala" | "swift" | "dart" | "julia" | "objc"
  | "solidity" | "protobuf" | "vue" | "svelte" | "r" -> Full
  | "ruby" | "php" | "haskell" | "lua" | "ada" | "sql"
  | "elixir" | "erlang" | "clojure" -> Imports
  | "markdown" | "pdf" | "image" -> Docs
  | "nix" | "perl" | "fsharp" | "groovy" | "powershell" | "fortran"
  | "pascal" | "graphql" | "terraform" | "nim" | "crystal" | "d" | "shell"
  | "verilog" | "vhdl" | "starlark" | "cmake" | "vb" | "asm" | "prolog" | "lisp"
  | "msbuild" | "sln" | "razor" | "xaml"
  | "elm" | "vala" | "cobol" | "abap" | "apex" | "matlab" -> Imports
  | "glsl" | "hlsl" | "opencl" -> Files
  | _ -> Files

let label = function
  | Full -> "symbols + imports + calls"
  | Imports -> "symbols + imports"
  | Docs -> "links"
  | Files -> "files only"
