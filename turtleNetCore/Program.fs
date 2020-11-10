open System

open OpenTK.Graphics.OpenGL;
open OpenTK.Graphics;

open Turtle
open OpenTK


let render (control : GameWindow) (args : FrameEventArgs) = 
    GL.Viewport(0, 0, control.ClientSize.Width, control.ClientSize.Height);

    GL.MatrixMode(MatrixMode.Projection)
    GL.LoadIdentity()
    GL.Ortho(0.0, 100.0, 0.0, 100.0, 0.0, 100.0)
    GL.MatrixMode(MatrixMode.Modelview)
    GL.LoadIdentity()

    GL.Clear(ClearBufferMask.ColorBufferBit);
    

    GL.Begin(PrimitiveType.LineStrip);

    let exampleProgram,desiredStart = PartOne.Examples.spiral
    // Test programm parser
    let programTestParser = Parser.Examples.programParserResult (Parser.parseProgram Parser.Examples.testStairs)
    let desiredStart = (50.0, 50.0)
    let points = PartOne.runTurtleProgram desiredStart programTestParser

    GL.Color3(1.0f, 1.0f, 1.0f) 
    for (x,y) in points do
        GL.Vertex2(x,y);

    GL.End();
    control.SwapBuffers()

[<EntryPoint;STAThread>]
let main argv = 
    use window = new GameWindow()

    window.RenderFrame.Add(fun a -> render window a)
    window.Run()

    0 
