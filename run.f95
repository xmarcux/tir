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

!Module containing functionality for starting/stopping 
!and handling of the running process.

module run
    
    implicit none

    contains

    !Function returns true if timer is running
    logical function running()
        logical :: ex

        inquire(file=".RUNNING", exist=ex)
        running=ex
        return
    end function running

    !Function returns time in days(1), hour(2), 
    !minutes(3) and seconds(4) that timer has been running for.
    !If timer is not running 0 is returned.
    !If an error occurred days is set to -1 and error
    !is written to terminal from this function
    function timeRunning() result(time)
        integer, dimension(4) :: time, diff
        integer, dimension(3) :: odate, otime, ndate, ntime, tdiff
        integer :: ios, days, sumodate, sumndate
        character(8) :: cdate
        character(10) :: ctime

        days = 0
        diff(1) = 0
        diff(2) = 0
        diff(3) = 0
        diff(4) = 0

        if (running()) then
            open(unit=20, file=".RUNNING", status="old", &
                 action="read", position="rewind", iostat=ios)

            if (ios == 0) then
                read(20,'(I4,I2,I2)') odate(1), odate(2), odate(3)
                read(20,'(I2,I2,I2)') otime(1), otime(2), otime(3)

                call date_and_time(cdate, ctime)
                read(cdate, '(I4,I2,I2)') ndate(1), ndate(2), ndate(3)
                read(ctime, '(I2,I2,I2)') ntime(1), ntime(2), ntime(3)

                if (odate(1) == ndate(1) .and. odate(2) == ndate(2) &
                    .and. odate(3) == ndate(3)) then
                    tdiff = timediff(otime(1), otime(2), otime(3), &
                                     ntime(1), ntime(2), ntime(3))
                else
                    days = dayDiff(odate(1), odate(2), odate(3), &
                                   ndate(1), ndate(2), ndate(3))
                    if (otime(1) > ntime(1) .or. &
                       (otime(1) == ntime(1) .and. otime(2) > ntime(2))) then
                        days = days - 1
                        ntime(1) = ntime(1) + 24
                    end if

                    tdiff = timediff(otime(1), otime(2), otime(3), &
                                     ntime(1), ntime(2), ntime(3))

                end if

                sumodate = odate(1) + odate(2) + odate(3)
                sumndate = ndate(1) + ndate(2) + ndate(3)

                if ((sumodate > sumndate) .or. &
                    (sumodate == sumndate .and. &
                    ((otime(1) > ntime(1)) .or. &
                     (otime(1) == ntime(1) .and. otime(2) > ntime(2)) .or. &
                     (otime(1) == ntime(1) .and. otime(2) == ntime(2) .and. &
                      otime(3) > ntime(3))))) then
                    write(*, '(3x,a)') "Error with running time, stopping timer."
                    diff(1) = -1
                    if (.not. stop()) then
                        write(*,'(3x,a)') "Error stopping timer."
                    end if
                else
                    diff(1) = days
                    diff(2) = tdiff(1)
                    diff(3) = tdiff(2)
                    diff(4) = tdiff(3)
                end if

            else
                write(*,'(3x,a)') "Error opening file: .RUNNING, stopping timer."
                diff(1) = -1
                if (.not. stop()) then
                    write(*,'(3x,a)') "Error stopping timer."
                end if
            end if

            close(20)

        end if

        time = diff
        return
    end function timeRunning


    !Function starts timer returns true 
    !if success otherwise false
    logical function start()
        character(8) :: date
        character(10) :: time
        integer :: ios

        if (running()) then
            start=.false.
        else
            call date_and_time(date, time)

            open(unit=20, file=".RUNNING", status="new", &
                 action="write", position="rewind", iostat=ios)

            if (ios == 0) then
                write(20,'(a8)') date
                write(20,'(a8)') time
                close(20)
                start=.true.
            else
                close(20)
                start=.false.
            end if

        end if


        return
    end function start

    !Function stops timer and removes file
    !.RUNNING and returns true if successful.
    !Remeber to save data to database if values are valid.
    logical function stop()
        integer :: ios
        character(8) :: odate, ndate
        character(6) :: otime, ntime, dirname
        character(27) :: filepath
        integer, dimension(4) :: timediff
        logical :: fileExists, errorFlag

        errorFlag = .true.

        call date_and_time(ndate, ntime)
        timediff = timeRunning()

        open(unit=20, file=".RUNNING", status="old", &
             action="read", position="rewind", iostat=ios)

        if (ios == 0) then
            read(20,*) odate
            read(20,*) otime
        else
            write(*,'(3x,a)') "Error opening file: .RUNNING"
            errorFlag = .false.
        end if

        close(20)        

        read(odate,'(a6)') dirname
        filepath = "tirdb/" // dirname // "/" // dirname // "_log.tir"

        ios = system("mkdir -p tirdb/" // dirname)
        if (.not. ios == 0) then
            write(*,'(3x,a)') "Error: mkdir -p tirdb/" // dirname
            errorFlag = .false.
        end if

        inquire(file=filepath, exist=fileExists)
        if (fileExists) then
            open(unit=22, file=filepath, status="old", &
                 action="write", position="append", iostat=ios)
        else
            open(unit=22, file=filepath, status="new", &
                 action="write", position="rewind", iostat=ios)
        end if

        if (ios == 0) then
            write(22,'(a8,a,a6,a,a8,a,a6,a,I4,a,I2,a,I2,a,I2)') &
                  odate, ":", otime, ":", ndate, ":", ntime, ":", &
                  timediff(1), ":", timediff(2), ":", timediff(3), ":", timediff(4)
        else
            write(*,'(3x,a)') "Error opening file: " // filepath
            errorFlag = .false.
        end if

        close(22)

        call unlink(".RUNNING", status=ios)

        if (.not. ios == 0) then
            write(*,'(3x,a)') "Error deleting file: .RUNNING"
            errorFlag = .false.
        end if 

        stop = errorFlag

        return
    end function stop


    !Returns hours, minutes and seconds 
    !between to times.
    function timediff(oldHour, oldMin, oldSec, &
                      newHour, newMin, newSec) result(time)
        integer, intent(inout) :: oldHour, oldMin, oldSec
        integer, intent(inout) :: newHour, newMin, newSec
        integer, dimension(3) :: time, diff

        diff(1) = newHour - oldHour
        diff(2) = newMin - oldMin

        if (diff(2) < 0) then
            diff(1) = diff(1) - 1
            diff(2) = 60 + diff(2)
        end if

        diff(3) = newSec - oldSec

        if (diff(3) < 0) then
            if (diff(2) == 0) then
                diff(1) = diff(1) - 1
                diff(2) = 59
            else
                diff(2) = diff(2) - 1
            end if
            diff(3) = 60 + diff(3)
        end if            

        time = diff

        return
    end function timediff

    !Returns number of days between dates.
    integer function dayDiff(oldYear, oldMonth, oldDay, &
                             newYear, newMonth, newDay)
        integer, intent(inout) :: oldYear, oldMonth, oldDay
        integer, intent(inout) :: newYear, newMonth, newDay
        integer :: diff

        diff = 0

        if (oldYear == newYear .and. oldMonth == newMonth &
            .and. oldDay < newDay) then
            diff = newDay - oldDay
        end if

        if (oldYear == newYear .and. oldMonth < newMonth) then
            diff = yearDays(newYear, newMonth, newDay) - &
                   yearDays(oldYear, oldMonth, oldDay)
        end if

        if (oldYear < newYear) then
            if (mod(oldYear, 4) == 0) then
                diff = 366 - yearDays(oldYear, oldMonth, oldDay) &
                           + yearDays(newYear, newMonth, newDay)
            else

            end if
        end if

        dayDiff = diff

        return
    end function dayDiff

    !Returns number of days from start of year
    integer function yearDays(year, month, day)
        integer, intent(in) :: year, month, day
        integer :: feb

        if (mod(year, 4) == 0) then
            feb = 29
        else
            feb = 28
        end if

        select case (month-1)
            case (0)
                yearDays = day
            case (1)
                yearDays = 31+day
            case (2)
                yearDays = 31+feb+day
            case (3)
                yearDays = 31+feb+31+day
            case (4)
                yearDays = 31+feb+31+30+day
            case (5)
                yearDays = 31+feb+31+30+31+day
            case (6)
                yearDays = 31+feb+31+30+31+30+day
            case (7)
                yearDays = 31+feb+31+30+31+30+31+day
            case (8)
                yearDays = 31+feb+31+30+31+30+31+31+day
            case (9)
                yearDays = 31+feb+31+30+31+30+31+31+30+day
            case (10)
                yearDays = 31+feb+31+30+31+30+31+31+30+31+day
            case (11)
                yearDays = 31+feb+31+30+31+30+31+31+30+31+30+day
        end select

        return
    end function yearDays

end module run
