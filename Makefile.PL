use inc::Module::Install;

my $version = $FusionInventory::Agent::Task::OcsDeploy;

name 'FusionInventory-Agent-Task-OcsDeploy';

include 'Module::AutoInstall';

abstract 'OCS Software deployment support for FusionInventory Agent';
license 'gpl';
version $version;

perl_version '5.006';
requires 'FusionInventory::Agent' => '2.0';
requires 'Digest::MD5' => undef;
requires 'XML::Simple' => undef;
requires 'File::Copy' => undef;
requires 'File::Glob' => undef;
requires 'File::Path' => undef;
requires 'File::Copy::Recursive' => undef;
requires 'Digest::MD5' => undef;
requires 'Archive::Extract' => undef;

WriteAll;

