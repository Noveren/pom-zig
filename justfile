
test:
    zig test -ODebug \
        ./src/root.zig
    zig test -ODebug \
        --dep pom -Mroot=./example/rgb.zig -Mpom=./src/root.zig
    zig test -ODebug \
        --dep pom -Mroot=./example/json.zig -Mpom=./src/root.zig