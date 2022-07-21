#!/usr/bin/env raku
use JSON::Fast;
use Test;

for dir('test/compile/') -> $ast-file {
  next unless $ast-file ~~ /'.ast.json'$/;
  my $name = $ast-file.Str.split('/')[*-1];
  my $module-name = $name.subst(/'.ast.json'/, '');
  my $bc-file = $ast-file.subst(/'.ast'/, '.bc');

  my $proc = Proc::Async.new(<<stack run $module-name $ast-file>>);
  $proc.stdout.tap({ $_ });
  $proc.stderr.tap({ $_ });
  await $proc.start;
  my %got-bc = from-json(slurp $module-name ~ '.undo-bc');

  my %expected-bc = from-json(slurp($bc-file));
  is-deeply %got-bc, %expected-bc, "File $ast-file";
}
