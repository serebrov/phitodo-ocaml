# phitodo-tui-ocaml

A TUI task manager with GitHub and Toggl integration, written in OCaml using the TEA (Elm Architecture) pattern.

## Features

- Full-featured task management (create, edit, delete, complete)
- Multiple views: Inbox, Today, Upcoming, Anytime, Completed, Review
- Project and tag organization
- GitHub integration (issues, PRs, review requests)
- Toggl time tracking integration
- Vim-style keyboard navigation
- SQLite persistence

## Requirements

- OCaml 5.1+
- opam (OCaml package manager)
- dune (build system)

## Installation

### 1. Install opam (if not installed)

```bash
# macOS
brew install opam

# Initialize opam
opam init
eval $(opam env)
```

### 2. Create a local switch (recommended)

```bash
cd phitodo-tui-ocaml
opam switch create . 5.1.0 --no-install
eval $(opam env)
```

### 3. Install dependencies

```bash
make install-deps
# or
opam install . --deps-only --with-test -y
```

### 4. Build

```bash
make build
# or
dune build
```

### 5. Run

```bash
make run
# or
dune exec phitodo
```

## Configuration

Configuration is stored in `~/.config/phitodo-tui/config.toml`:

```toml
shortcut_modifier = "alt"
github_token = "ghp_..."
github_repos = ["owner/repo1", "owner/repo2"]
toggl_token = "..."
toggl_hidden_projects = []
```

### GitHub Token

1. Go to GitHub → Settings → Developer settings → Personal access tokens
2. Generate a token with `repo` scope
3. Add to config file

### Toggl Token

1. Go to Toggl Track → Profile Settings
2. Scroll down to API Token
3. Add to config file

## Keyboard Shortcuts

### Navigation
- `1-9` - Switch views (Inbox, Today, Upcoming, etc.)
- `Tab` - Cycle focus (Sidebar → List → Detail)
- `Shift+Tab` - Reverse cycle
- `h/l` or `←/→` - Focus left/right
- `j/k` or `↓/↑` - Navigate list
- `g` - Jump to first item
- `G` - Jump to last item

### Task Actions
- `Space` - Toggle complete
- `n` - New task
- `e` - Edit task
- `d` - Delete task
- `o` - Open task URL

### Status
- `i` - Set to Inbox
- `a` - Set to Active
- `s` - Set to Scheduled

### Priority (Ctrl+number)
- `Ctrl+1` - None
- `Ctrl+2` - Low
- `Ctrl+3` - Medium
- `Ctrl+4` - High

### Other
- `r` - Refresh data
- `?` - Show help
- `q` - Quit

## Project Structure

```
phitodo-tui-ocaml/
├── bin/main.ml              # Entry point
├── lib/
│   ├── models/              # Data models (Task, Project, Tag)
│   ├── db/                  # Database (SQLite)
│   ├── services/            # Business logic (Filter, GitHub, Toggl)
│   ├── config/              # Configuration (TOML)
│   └── ui/
│       ├── components/      # Reusable UI components
│       └── views/           # Screen views
└── test/                    # Tests
```

## Technology Stack

| Component | Library |
|-----------|---------|
| TUI Framework | minttea + leaves |
| Database | sqlite3-ocaml |
| HTTP Client | cohttp-eio |
| JSON | yojson + ppx_deriving_yojson |
| TOML Config | otoml |
| Date/Time | ptime + timedesc |
| UUID | uuidm |

## Development

```bash
# Build
dune build

# Run tests
dune runtest

# Format code
dune fmt

# Watch mode (rebuild on changes)
dune build --watch
```

## License

MIT
