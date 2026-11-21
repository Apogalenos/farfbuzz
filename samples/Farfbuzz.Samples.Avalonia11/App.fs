namespace Farfbuzz.Samples.Avalonia11

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Media

/// <summary>The application the samples run: no theme; on a head with a single view it shows the corpus, in a headless run it has no view.</summary>
type App() =
    inherit Application()

    /// <summary>Shows the corpus lines, each in its own embedded family, when the platform has a single view (the browser head).</summary>
    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? ISingleViewApplicationLifetime as lifetime ->
            let panel = StackPanel()
            for line in Corpus.lines do
                panel.Children.Add(TextBlock(Text = line.Text, FontSize = 24.0, FontFamily = FontFamily($"avares://Farfbuzz.Samples.Avalonia11/Assets#{line.Family}")))
            lifetime.MainView <- panel
        | _ -> ()
        base.OnFrameworkInitializationCompleted()
