// RUN: %aslrun %s
// Copyright (C) 2025-2025 Intel Corporation

func FUT(x : bits(4))
begin
    asl_end_execution(x == '0000');
end

func main() => integer
begin
    // this will exit with success
    FUT('0000');

    return 1;
end
