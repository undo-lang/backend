#!/usr/bin/env raku
use JSON::Fast;
use Test;

my @specs = from-json(slurp 'test/compile/spec.json');
for @specs -> % (:$name, :$is-error) {
  my $ast-file = "test/compile/$name.ast.json";
  ok $ast-file.IO ~~ :e, "AST file for $name exists";

  my $error;
  my $proc = Proc::Async.new(<<stack run $name $ast-file>>);
  $proc.stdout.tap({ $_ });
  $proc.stderr.tap({ $error ~= $_ });
  await $proc.start;

  my $generated = "$name.undo-bc".IO;
  LEAVE try { unlink $generated; }

  if $is-error {
    ok $generated ~~ :!e, "Error file for $name exists";
    my $expected = slurp "test/compile/$name.error";
    ok $error ~~ /$error/, "Error for $name matches the expected message";
  } else {
    if $generated ~~ :e {
      pass "BC file for $name exists";
      my %generated = from-json(slurp $generated);

      my $bc-file = "test/compile/$name.bc.json";
      my %expected-bc = from-json(slurp $bc-file);
      is-deeply %generated, %expected-bc, "Correct BC generated for $name";
    } else {
      fail "Expected to succeed, but errored instead: $error";
    }
  }
}

done-testing;
