// RUN: %asli --batchmode %s || filecheck %s
// Copyright (C) 2023-2025 Intel Corporation

```json
name: main
shortdesc: Main entrypoint
longdesc: >
  Main entrypoint into the specification.
```

func main() => integer
begin
    return 0;
end
