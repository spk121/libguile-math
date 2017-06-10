#!/usr/bin/perl
# unidata_to_charset.pl --- Extract numerical values UnicodeData.txt
#
# Copyright (C) 2009, 2010 Free Software Foundation, Inc.
# Copyright (C) 2017 Michael L. Gran
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

open(my $in,  "<",  "UnicodeData.txt")  or die "Can't open UnicodeData.txt: $!";
open(my $out, ">",  "numval.scm") or die "Can't open numval.scm: $!";

# The procedure generates the two C structures necessary to describe a
# given category.
sub compute {
    my($f) = @_;
    my $start = -1;
    my $end = -1;
    my $len = 0;
    my @rstart = (-1);
    my @rend = (-1);
    my $numeric;

    seek($in, 0, 0) or die "Can't seek to beginning of file: $!";

    print "$f\n";

    while (<$in>) {
        # Parse the 14 column, semicolon-delimited UnicodeData.txt
        # file
        chomp;
        my(@fields) = split(/;/);

        # The codepoint: an integer
        my $codepoint = hex($fields[0]);

        # If this is a character range, the last character in this
        # range
        my $codepoint_end = $codepoint;

        # The name of the character
        my $name = $fields[1];

        # A two-character category code, such as Ll (lower-case
        # letter)
        my $category = $fields[2];

        # The codepoint of the uppercase version of this char
        $numeric = $fields[8];

        if ($numeric ne "") {
            # Some pairs of lines in UnicodeData.txt delimit ranges of
            # characters.
            $start = $codepoint;
            if ($name =~ /First/) {
                $line = <$in>;
                die $! if $!;
                $end = hex( (split(/;/, $line))[0] );
            } else {
                $end = $codepoint + 1;
            }
            for (my $n=$start; $n<$end; $n++) {
		if ($n < 128) {
		    printf $out "    (#\\%c . %s)\n", $n, $numeric;
		} else {
		    printf $out "    (#\\x%04x . %s)\n", $n, $numeric;
		}
            }
        }
    }
}

# Write a bit of a header
print $out <<"END";
;;; -*- mode: scheme; coding: us-ascii; indent-tabs-mode: nil; -*-
;;; (mlg numval) - the Unicode numerical values associated with codepoints
;;; Copyright (C) 2017 Michael L. Gran <spk121\@yahoo.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundataion, either version 3 of
;;; this License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>


;;; This file was generated from
;;;   http://unicode.org/Public/UNIDATA/UnicodeData.txt
;;;   with the unidata_to_numval.pl script.

(define-module (mlg numval)
  #:export (*unicode-numval-alist*))

(define *unicode-numval-alist*
  '(
END
compute;
print $out "    ))\n\n";

close $in;
close $out;

# And we're done.
