!**********************************************************************
!Copyright (C) 2015 Marcus Pedersén
!
!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.
!**********************************************************************

!This is the main file and where the program starts.
!tir is a timer that keeps track on time passed.

!Arguments defined in tir:
! no argument   : Status if timer is stopped or if it is running.
! -s or --start : Starts a new timer period if tir is stopped.
! -e or --stop  : Stops running period of time if timer is running.
! -q date date  : Returns report on time registered between dates. 
!                 Date format YYYYMMDD.
! -l            : Shows licence and exit program.
! -h            : Shows help text and exit program.

program tir
    use run

    implicit none

    integer :: noArgs
    character(7) :: arg1
    character(6) :: format
    character(8) :: date
    character(6) :: time
    integer, dimension(4) :: timePassed

    format = "(3x,a)"

    noArgs = command_argument_count()
 
    if (noArgs == 0) then
        if (running()) then
            timePassed = timeRunning()
            if (timePassed(1) >= 0) then
                write(*, format) "Timer is running."
                write(*, format) "Timer has been running for:"

                write(*,'(3x,I3,a,I2,a,I2,a,I2,a)') &
                      timePassed(1), " days, ", timePassed(2), &
                      " hours, ", timePassed(3), " minutes, ", &
                      timePassed(4), " seconds"
                write(*, format) "Use -e or --stop to stop timer."
            end if
        else
            write(*, format) "Timer is stopped."
            write(*, format) "Use -s or --start to start timer."
        end if
    end if

    if (noArgs == 1) then
        call get_command_argument(1, arg1)

        if (trim(arg1) == '-l') then
            call license()
        else if (trim(arg1) == '-h') then
            call help()
        else if (trim(arg1) == '-s' .or. &
                 trim(arg1) == '--start') then
            if (.not. start()) then
                write(*,format) "Error starting timer"
                if (running()) then
                    write(*,format) "Timer is already running"
                    write(*, format) "Use -e or --stop to stop timer."
                end if
            else
                call date_and_time(date, time)
                write(*,format) "Timer started at: " // date // " " // time
            end if
        else if (trim(arg1) == '-e' .or. &
                 trim(arg1) == '--stop') then
            if (running()) then
                if (.not. stop()) then
                    write(*,format) "Error stopping timer."
                else
                    call date_and_time(date, time)
                    write(*,format) "Timer stopped at: " // date // " " // time
                end if
            else
                write(*,format) "Timer is already stopped"
                write(*,format) "Use -s or --start to start timer."
            end if
        else
            write(*, format) "Wrong argument"
            write(*, format) "Try: tir -h"
            write(*, format) "for help."
        end if
    else
        !This is a search for dates in db.
    end if


contains

    !Subroutine is called if flag -l is used as argument
    subroutine license()
        implicit none

        character(7) :: format

        format = "(3X, a)"

        write(*, '(/,3x,a,/)') "tir - WorkTimer"
        write(*, '(3x,a,/)') "Copyright (C) 2015 Marcus Pedersén"
        write(*,format) "This program is free software: you can redistribute it and/or modify"
        write(*,format) "it under the terms of the GNU General Public License as published by"
        write(*,format) "the Free Software Foundation, either version 3 of the License, or"
        write(*,'(3X,a,/)') "(at your option) any later version."
        write(*,format) "This program is distributed in the hope that it will be useful,"
        write(*,format) "but WITHOUT ANY WARRANTY; without even the implied warranty of"
        write(*,format) "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
        write(*,'(3X,a,/)') "GNU General Public License for more details."
        write(*,format) "You should have received a copy of the GNU General Public License"
        write(*,'(3X,a,/)') "along with this program.  If not, see <http://www.gnu.org/licenses/>."

    end subroutine license

    !Subroutine is called if flag -h is used as argument
    subroutine help()
        implicit none

        character(7) :: format, format2

        format = "(3x,a)"
        format2 = "(6x,a)"

        write(*, '(/,3x,a,/)') "tir - WorkTimer"
        write(*, format) "Available arguments:"
        write(*, format) "-h"
        write(*, format2) "Help text. Description of application functionality."
        write(*, format2) "You used this flag to come here."
        write(*, format) "-l"
        write(*, format2) "License information."
        write(*, format) "-s, --start"
        write(*, format2) "Starting timer if it is not running"
        write(*, format) "-e, --stop"
        write(*, format2) "Stopping timer if it is running"
        write(*, format) "-q DATE DATE (option: FILE)"
        write(*, format2) "Returns report on time registered between dates."
        write(*, format2) "Date format YYYYMMDD." 
        write(*, format2) "If option FILE is given, then report is written to file."
        write(*,*) ""

    end subroutine help

end program tir
