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
    want ["_build/test/coverage-report/index.html"]
    want ["_build/release/examples/Observer/observer_example" <.> exe]

    let gtestDir = ".." </> "gtest"
    let gtestInclude = " -I " ++ (gtestDir </> "include") ++ " -I " ++ gtestDir
    let mockppDir = ".." </> "mock"
    let mockppInclude = " -I " ++ (mockppDir </> "include")
    let checkedCxxflags = "--coverage -std=c++11 -stdlib=libc++ -O0 -I ./include " ++ gtestInclude ++ mockppInclude
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

    --test targets

    let testlink = "-lstdc++ " ++ (gtestDir </> "src" </> "gtest-all.cc")

    "_build/test/coverage-report/index.html" *> \out -> do
        need ["_build/test/patterntest" <.> exe]

        () <- cmd "mkdir -p _build/test/coverage-report"
        () <- cmd "lcov -no-external -b . --directory _build --zerocounters"
        () <- cmd "_build/test/patterntest"
        () <- cmd "lcov -no-external -b . --directory _build --capture --output-file _build/test/coverage-report/pattern.info"
        cmd "genhtml --no-branch-coverage -o _build/test/coverage-report _build/test/coverage-report/pattern.info"

    "_build/test/patterntest" <.> exe *> \out -> do
        cs <- getDirectoryFiles "" ["test//*.cpp"]
        let os = ["_build" </> "checked" </> c -<.> "o" | c <- cs]
        need os
        cmd "clang++" checkedCxxflags ldflags "-o" [out] os testlink
