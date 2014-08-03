import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

buildCxxObject :: String -> FilePath -> Action ()
buildCxxObject compileFlags out = do
    let c = dropDirectory1 $ dropDirectory1 $ out -<.> "cpp"
    let m = out -<.> "m"
    () <- cmd "clang++" compileFlags "-c" [c] "-o" [out] "-MMD -MF" [m]
    needMakefileDependencies m

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    want ["_build/release/examples/Observer/observer_example" <.> exe]

    let gtestDir = ".." </> "gtest"
    let checkedCxxflags = "--coverage -std=c++11 -stdlib=libc++ -O0 -I ./include -I " ++ (gtestDir </> "include") ++ " -I " ++ gtestDir
    let releaseCxxflags = "-std=c++11 -stdlib=libc++ -O3 -I ./include"

    phony "clean" $ do
        removeFilesAfter "_build" ["//*"]

    --build an object file for release
    "_build/release//*.o" *> (buildCxxObject releaseCxxflags)

    --build an object file for checked
    "_build/checked//*.o" *> (buildCxxObject checkedCxxflags)

    --example targets

    let ldflags = ""
    let examplelink = "-lstdc++"

    "_build/release/examples/Observer/observer_example" <.> exe *> \out -> do
        cs <- getDirectoryFiles "" ["examples/Observer//*.cpp"]
        let os = ["_build" </> "release" </> c -<.> "o" | c <- cs]
        need os
        cmd "clang++" checkedCxxflags ldflags "-o" [out] os examplelink
