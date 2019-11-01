Can use --export to export names

  $ amc compile tests/amc/files/export.ml
  do
  
  end

  $ amc compile tests/amc/files/export.ml --export
  do
    local None = { __tag = "None" }
    local function Some(a)
      return { __tag = "Some", a }
    end
    local function f(tmp) return 0 end
    local function g(tmp) return 0 end
    return {
      M = { f = f },
      N = { f = f, g = g },
      None = None,
      Some = Some,
      x = 0
    }
  end

  $ amc compile tests/amc/files/export.ml --export -O0
  do
    local function f(tmp) return 0 end
    local function g(tmp) return 0 end
    return {
      M = { f = f },
      N = { f = f, g = g },
      x = 0
    }
  end
