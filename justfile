
test:
    zig test ./src/root.zig
#   && zig test --dep pom -Mroot=./leptjson/parser.zig -Mpom=./src/root-bak-bak.zig

# test_bak:
#     zig test -ODebug \
#         ./src/root.zig
#     zig test -ODebug \
#         --dep pom -Mroot=./example/rgb.zig -Mpom=./src/root-bak.zig
#     zig test -ODebug \
#         --dep pom -Mroot=./example/json.zig -Mpom=./src/root-bak.zig