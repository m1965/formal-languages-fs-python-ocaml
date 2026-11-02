/// Graphviz is a native CLI boundary; arguments and graph source never pass through a shell.
let renderDot (dot: string) =
    let settings = ProcessStartInfo("dot")
    settings.ArgumentList.Add "-Tsvg"
    settings.UseShellExecute <- false
    settings.RedirectStandardInput <- true
    settings.RedirectStandardOutput <- true
    settings.RedirectStandardError <- true
    use graphviz = new Process(StartInfo = settings)
    try graphviz.Start() |> ignore
    with error -> failwithf "Install Graphviz and put dot on PATH: %s" error.Message
    let output = graphviz.StandardOutput.ReadToEndAsync()
    let errors = graphviz.StandardError.ReadToEndAsync()
    graphviz.StandardInput.Write dot
    graphviz.StandardInput.Close()
    graphviz.WaitForExit()
    let svg = output.GetAwaiter().GetResult()
    if graphviz.ExitCode <> 0 then failwithf "Graphviz failed: %s" (errors.GetAwaiter().GetResult())
    svg
