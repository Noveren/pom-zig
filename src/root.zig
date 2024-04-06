
const std = @import("std");

const typeCheck = @import("typeCheck.zig");

const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const Result = parser.Result;

pub const U8 = @import("U8.zig");
test { std.testing.refAllDecls(U8); _ = U8; }

const choice = @import("choice.zig");
pub const Choice = choice.Choice;
pub const ChoicePrefix = choice.ChoicePrefix;

pub const ErrorNOP = error {};
pub const ErrorEOF = error { EOF };
pub const ErrorFailedToMatchLiteral = error { FailedToMatchLiteral };
pub const ErrorFailedToPred = error { FailedToPred };
pub const ErrorFailedToMap = error { FailedToMap };
pub const ErrorFailedToChoice = error { FailedToChoice };
pub const Error = 
       ErrorNOP
    || ErrorEOF
    || ErrorFailedToMatchLiteral
    || ErrorFailedToPred
    || ErrorFailedToMap
    || ErrorFailedToChoice
;