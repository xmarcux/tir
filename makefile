#Makefile for application tir - WorkTimer

#######################################################################
#Copyright (C) 2015 Marcus Pedersén
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.
#######################################################################

CC = gfortran
CFLAGS = -c -Wall

all: tir

tir: tir.o run.o report.o
	$(CC) tir.o run.o report.o -o tir

tir.o: tir.f95 run.o report.o
	$(CC) $(CFLAGS) tir.f95

report.o: report.f95 run.o
	$(CC) $(CFLAGS) report.f95

run.o: run.f95
	$(CC) $(CFLAGS) run.f95

clean:
	rm *.o *~ *.mod fort.* tir
	rm -fr tirdb
