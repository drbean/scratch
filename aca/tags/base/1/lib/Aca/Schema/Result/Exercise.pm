use utf8;
package Aca::Schema::Result::Exercise;

# Created by DBIx::Class::Schema::Loader
# DO NOT MODIFY THE FIRST PART OF THIS FILE

=head1 NAME

Aca::Schema::Result::Exercise

=cut

use strict;
use warnings;

use Moose;
use MooseX::NonMoose;
use MooseX::MarkAsMethods autoclean => 1;
extends 'DBIx::Class::Core';

=head1 COMPONENTS LOADED

=over 4

=item * L<DBIx::Class::InflateColumn::DateTime>

=item * L<DBIx::Class::TimeStamp>

=back

=cut

__PACKAGE__->load_components("InflateColumn::DateTime", "TimeStamp");

=head1 TABLE: C<exercise>

=cut

__PACKAGE__->table("exercise");

=head1 ACCESSORS

=head2 type

  data_type: 'text'
  is_nullable: 0

=head2 genre

  data_type: 'text'
  is_nullable: 0

=head2 id

  data_type: 'text'
  is_nullable: 0

=head2 description

  data_type: 'text'
  is_nullable: 0

=cut

__PACKAGE__->add_columns(
  "type",
  { data_type => "text", is_nullable => 0 },
  "genre",
  { data_type => "text", is_nullable => 0 },
  "id",
  { data_type => "text", is_nullable => 0 },
  "description",
  { data_type => "text", is_nullable => 0 },
);

=head1 PRIMARY KEY

=over 4

=item * L</genre>

=item * L</id>

=back

=cut

__PACKAGE__->set_primary_key("genre", "id");


# Created by DBIx::Class::Schema::Loader v0.07033 @ 2013-10-20 13:52:55
# DO NOT MODIFY THIS OR ANYTHING ABOVE! md5sum:hlS9+tMY4oGwI+28dfayuA


# You can replace this text with custom code or comments, and it will be preserved on regeneration
__PACKAGE__->meta->make_immutable;
1;
