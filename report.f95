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

!Module contains functionallity for making reports.

module report
    use run

    implicit none

    !Type for a timed period
    type timed
        character(8) :: startDate
        character(6) :: startTime
        character(8) :: stopDate
        character(6) :: stopTime
        integer :: days
        integer :: hours
        integer :: mins
        integer :: secs
    end type timed

    contains

    !Prints a report on todays time.
    subroutine today(toFile, filename)
        logical, intent(in) :: toFile
        character(100), intent(in) :: filename
        character(8) :: date, date2

        call date_and_time(date)
        date2 = date
        call searchTime(date, date2, toFile, filename)

    end subroutine today

    !Prints a report on yesterdays time
    subroutine yesterday(toFile, filename)
        logical, intent(in) :: toFile
        character(100), intent(in) :: filename
        character(8) :: date, date2

        call date_and_time(date)
        date = prevDate(date)
        date2 = date
        call searchTime(date, date2, toFile, filename)

    end subroutine yesterday

    !Prints a report on this months time
    subroutine thisMonth(toFile, filename)
        logical, intent(in) :: toFile
        character(100), intent(in) :: filename
        character(8) :: date, date2

        call date_and_time(date)
        date2(1:6) = date(1:6)
        date2(7:8) = "01"
        call searchTime(date2, date, toFile, filename)

    end subroutine thisMonth

    !Returns a report on last months time
    subroutine lastMonth(toFile, filename)
        logical, intent(in) :: toFile
        character(100), intent(in) :: filename
        character(8) :: date, date2
        integer :: month, year

        call date_and_time(date)
   
        read(date(1:4),'(I4.4)') year
        read(date(5:6),'(I2.2)') month
        month = month -1

        if (month < 1) then
            date(5:8) = "1201"
            year = year - 1
            write(date(1:4),'(I4.4)') year
            date2(1:6) = date(1:6)
            date2(7:8) = "31"
        else
            write(date(5:6),'(I2.2)') month
            date(7:8) = "01"
            date2(1:6) = date(1:6)
            if (month == 2 .and. mod(year,4) == 0) then
                date2(7:8) = "29"
            else if (month == 2) then
                date2(7:8) = "28"
            else if (month == 4 .or. month == 6 .or. &
                     month == 9 .or. month == 11) then
                date2(7:8) = "30"
            else
                date2(7:8) = "31"
            end if
        end if

        write(*,*) date, " ", date2

        call searchTime(date, date2, toFile, filename)

    end subroutine lastMonth

    !Returns true if date is correct.
    !Date between now and 2000-01-01
    logical function checkDate(date)
        character(8), intent(in) :: date
        character(8) :: dateNow
        integer :: year, month, day, yearNow, monthNow, dayNow
        logical :: dateOk

        dateOk = .false.

        call date_and_time(dateNow)

        read(date(1:4),'(I4)') year
        read(date(5:6),'(I2)') month
        read(date(7:8),'(I2)') day
        read(dateNow(1:4),'(I4)') yearNow
        read(dateNow(5:6),'(I2)') monthNow
        read(dateNow(7:8),'(I2)') dayNow

        if (year >= 2000 .and. year <= yearNow) then
            if (month == 2 .and. mod(year, 4) == 0 .and. day <= 29) then
                dateOk = .true.
            else if ((month == 4 .or. month == 6 .or. &
                     month == 9 .or. month == 11) .and. day <= 30) then
                dateOk = .true.
            else if (day <= 31) then
                dateOk = .true.
            end if
        end if

        if (dateOk .and. year == yearNow .and. &
            month == monthNow .and. day > dayNow) then
            dateOk = .false.
        else if (dateOk .and. year == yearNow .and. &
                 month > monthNow) then
            dateOk = .false.
        end if
  
        checkDate = dateOk

    end function checkDate


    subroutine searchTime(fromDate, toDate, toFile, fileName)
        character(8), intent(in) :: toDate
        character(8), intent(inout) :: fromDate
        logical, intent(in) :: toFile
        character(100), intent(in) :: fileName
        logical :: fileExists, fileOpen, writeToFile, isOpen
        integer :: ios, iosw, noTimes, iMonth, iYear
        integer, dimension(4) :: timeNow, totTime, monthTime
        character(27) :: filepath
        character(6) :: date, tTime, nowTime
        character(8) :: tDate, nowDate
        character(39) :: readF, writeF
        character(24) :: headF
        type(timed) :: t

        readF = '(a8,x,a6,x,a8,x,a6,x,I4,x,I2,x,I2,x,I2)'
        writeF = '(x,a8,2x,a6,4x,a8,2x,a6,4x,I8,I8,I8,I8)'
        headF = '(/,a17,a20,a12,a8,a8,a8)'

        noTimes = 0
        date = "0"

        timeNow(1) = 0
        timeNow(2) = 0
        timeNow(3) = 0
        timeNow(4) = 0

        totTime(1) = 0
        totTime(2) = 0
        totTime(3) = 0
        totTime(4) = 0

        monthTime(1) = 0
        monthTime(2) = 0
        monthTime(3) = 0
        monthTime(4) = 0

        writeToFile = toFile

        if (writeToFile) then
            inquire(file=fileName, exist=fileExists)
            if (fileExists) then
                if (overwriteReport(fileName)) then
                    open(unit=88, file=fileName, status='old', &
                         action='write', position='rewind', iostat=iosw)
                else
                    stop
                end if
            else
                open(unit=88, file=fileName, status='new', &
                     action='write', position='rewind', iostat=iosw)
            end if

            if (iosw /= 0) then
                write(*,'(x,a,a)') "Error opening file: ", fileName
                write(*,'(x,a,/)')   "Writes report to terminal."
                writeToFile = .false.
            end if
        end if

        call date_and_time(tDate, tTime)

        if (running() .and. toDate >= tDate) then
            timeNow = timeRunning()
            noTimes = 1

            open(unit=25, file=".RUNNING", status='old', &
                 action='read', position='rewind', iostat=ios)

            if (ios == 0) then
                read(25,*) nowDate
                read(25,*) nowTime
            end if
            
            close(25)

            if (nowDate < tDate) then
                timeNow(1) = 0
                read(tTime,'(I2)') timeNow(2)
                read(tTime,'(2x,I2)') timeNow(3)
                read(tTime,'(4x,I2)') timeNow(4)
            end if

            totTime = totTime + timeNow
        end if

        if (writeToFile) then
            write(88, headF) "Start Date", "End Date", "Days", "Hours", "Mins", "Secs"
            write(88,'(x,a)') &
                  "------------------------------------------------------------------------"
        else
            write(*, headF) "Start Date", "End Date", "Days", "Hours", "Mins", "Secs"
            write(*,'(x,a)') &
                  "------------------------------------------------------------------------"
        end if

        if (date == "0") then
            read(fromDate,'(a6)') date
        end if

        do
            if(fromDate <= toDate) then
                read(fromDate,'(a6)') date
                filepath = "tirdb/" // date // "/" // date // "_log.tir"

                inquire(file=filepath, exist=fileExists)
                if (fileExists) then
                    inquire(file=filepath, opened=fileOpen)

                    if (.not. fileOpen) then
                        open(unit=20, file=filepath, status='old', &
                             action='read', position='rewind', iostat=ios)
                    end if

                    if (ios == 0) then
                        do
                            read(20, readF, iostat=ios) t%startDate, t%startTime, &
                                                        t%stopDate, t%stopTime, &
                                                        t%days, t%hours, t%mins, t%secs
                            if (ios < 0) then
                                read(fromDate(5:6), '(I2.2)') iMonth
                                iMonth = iMonth + 1
                                if (iMonth >= 13) then
                                    read(fromDate(1:4), '(I4)') iYear
                                    iYear = iYear + 1
                                    iMonth = 1
                                    write(fromDate(1:4),'(I4.4)') iYear
                                end if

                                write(fromDate(5:6),'(I2.2)') iMonth

                                close(20)
                                exit
                            else
                                if (fromDate <= t%startDate .and. &
                                    toDate >= t%stopDate) then
                                    if (writeToFile) then
                                        write(88,writeF) t
                                    else
                                        write(*,writeF) t
                                    end if

                                    noTimes = noTimes + 1
                                    totTime(1) = totTime(1) + t%days
                                    totTime(2) = totTime(2) + t%hours
                                    totTime(3) = totTime(3) + t%mins
                                    totTime(4) = totTime(4) + t%secs
                                    monthTime(1) = monthTime(1) + t%days
                                    monthTime(2) = monthTime(2) + t%hours
                                    monthTime(3) = monthTime(3) + t%mins
                                    monthTime(4) = monthTime(4) + t%secs
                                else if (fromDate > t%startDate .and. &
                                         fromDate == t%stopDate) then
                                    if (writeToFile) then
                                        write(88,writeF) t
                                    else
                                        write(*,writeF) t
                                    end if

                                    noTimes = noTimes + 1
                                    read(t%stopTime,'(I2,I2,I2)') t%hours, t%mins, t%secs
                                    totTime(2) = totTime(2) + t%hours
                                    totTime(3) = totTime(3) + t%mins
                                    totTime(4) = totTime(4) + t%secs
                                    monthTime(2) = monthTime(2) + t%hours
                                    monthTime(3) = monthTime(3) + t%mins
                                    monthTime(4) = monthTime(4) + t%secs
                                else if (toDate == t%startDate .and. &
                                         toDate < t%stopDate) then
                                    if (writeToFile) then
                                        write(88,writeF) t
                                    else
                                        write(*,writeF) t
                                    end if

                                    noTimes = noTimes + 1
                                    read(t%startTime,'(I2,I2,I2)') t%hours, t%mins, t%secs
                                        if (t%hours == 0 .and. t%mins == 0 .and. t%secs == 0) then
                                            t%hours = 24
                                        else if (t%mins == 0 .and. t%secs == 0) then
                                            t%hours = 24 - t%hours
                                        else if (t%mins == 0) then
                                            t%hours = 23 - t%hours
                                            t%mins = 59
                                            t%secs = 60 - t%secs
                                        else if (t%secs == 0) then
                                            t%hours = 23 - t%hours
                                            t%mins = 60 - t%mins
                                        else
                                            t%hours = 23 - t%hours
                                            t%mins = 59 - t%mins
                                            t%secs = 60 - t%secs
                                        end if
                                    totTime(2) = totTime(2) + t%hours
                                    totTime(3) = totTime(3) + t%mins
                                    totTime(4) = totTime(4) + t%secs
                                    monthTime(2) = monthTime(2) + t%hours
                                    monthTime(3) = monthTime(3) + t%mins
                                    monthTime(4) = monthTime(4) + t%secs
                                end if
                            end if
                        end do
                    else
                        write(*,'(3x,a)') "Error opening file: " // filepath
                        write(*,'(3x,a)') " Can not create report"
                    end if
            
                    fromDate = nextDate(fromDate)

                    if (fromDate(1:6)  /= date .and. fromDate <= toDate) then
                        date = fromDate(1:6)
                        close(20)
                        if (writeToFile) then
                            write(88,'(x,a)') &
                                  "------------------------------------------------------------------------"
                            write(88,'(3x,a13,25x,I8,I8,I8,I8,/)') "Total month: ", monthTime
                        else
                            write(*,'(x,a)') &
                                  "------------------------------------------------------------------------"
                            write(*,'(3x,a13,25x,I8,I8,I8,I8,/)') "Total month: ", monthTime
                        end if

                        monthTime(1) = 0
                        monthTime(2) = 0
                        monthTime(3) = 0
                        monthTime(4) = 0
                        if (writeToFile) then
                            write(88,'(x,a)') &
                                  "------------------------------------------------------------------------"
                        else
                            write(*,'(x,a)') &
                                  "------------------------------------------------------------------------"
                        end if

                    end if
                else
                    read(fromDate(5:6), '(I2.2)') iMonth
                    iMonth = iMonth + 1
                    if (iMonth >= 13) then
                        read(fromDate(1:4), '(I4)') iYear
                        iYear = iYear + 1
                        iMonth = 1
                        write(fromDate(1:4),'(I4.4)') iYear
                    end if

                    write(fromDate(5:6),'(I2.2)') iMonth
                    fromDate(7:8) = "01"

                end if
            else
                inquire(file=filepath, opened=fileOpen)
                if (fileOpen) then
                    close(20)
                end if
                exit
            end if
        end do

        if (timeNow(1) /= 0 .or. timeNow(2) /= 0 .or. &
            timeNow(3) /= 0 .or. timeNow(4) /= 0) then
            if (writeToFile) then
                write(88,writeF) nowDate, nowTime, "Running", " ", timeNow(1), &
                                timeNow(2), timeNow(3), timeNow(4)
            else
                write(*,writeF) nowDate, nowTime, "Running", " ", timeNow(1), &
                                timeNow(2), timeNow(3), timeNow(4)
            end if
        end if

        if (noTimes == 0) then
            if (writeToFile) then
                write(88,'(3x,a)') "No records found"
            else
                write(*,'(3x,a)') "No records found"
            end if
        end if

        totTime(3) = totTime(3) + (totTime(4)/60)
        totTime(4) = mod(totTime(4),60)
        totTime(2) = totTime(2) + (totTime(3)/60)
        totTime(3) = mod(totTime(3),60)
        totTime(1) = totTime(1) + (totTime(2)/24)
        totTime(2) = mod(totTime(2),24)

        if (writeToFile) then
            write(88,'(x,a)') &
                          "------------------------------------------------------------------------"
        else
            write(*,'(x,a)') &
                          "------------------------------------------------------------------------"
        end if

        monthTime(1) = monthTime(1) + timeNow(1)
        monthTime(2) = monthTime(2) + timeNow(2)
        monthTime(3) = monthTime(3) + timeNow(3)
        monthTime(4) = monthTime(4) + timeNow(4)

        monthTime(3) = monthTime(3) + (monthTime(4)/60)
        monthTime(4) = mod(monthTime(4),60)
        monthTime(2) = monthTime(2) + (monthTime(3)/60)
        monthTime(3) = mod(monthTime(3),60)
        monthTime(1) = monthTime(1) + (monthTime(2)/24)
        monthTime(2) = mod(monthTime(2),24)        
     
        if (writeToFile) then
            write(88,'(3x,a13,25x,I8,I8,I8,I8,/)') "Total month: ", monthTime
            write(88,'(3x,a,I6)') "Number of records found: ", noTimes
            write(88,'(/,3x,a)') "Total time recorded: "
            write(88,'(3x,a9,I4,/,3x,a9,I4,/,3x,a9,I4,/,3x,a9,I4,/)') &
                  "Days:    ", totTime(1), "Hours:   ", totTime(2), &
                  "Minutes: ", totTime(3), "Seconds: ", totTime(4)
        else
            write(*,'(3x,a13,25x,I8,I8,I8,I8,/)') "Total month: ", monthTime
            write(*,'(3x,a,I6)') "Number of records found: ", noTimes
            write(*,'(/,3x,a)') "Total time recorded: "
            write(*,'(3x,a9,I4,/,3x,a9,I4,/,3x,a9,I4,/,3x,a9,I4,/)') &
                  "Days:    ", totTime(1), "Hours:   ", totTime(2), &
                  "Minutes: ", totTime(3), "Seconds: ", totTime(4)
        end if

        inquire(file=fileName, opened=isOpen)

        if (isOpen) then
            close(88)
            write(*, '(x,a,a)') "Report written to file: ", fileName
        end if

    end subroutine searchTime

    !Returns next date as a string.
    character(8) function nextDate(date)
        character(8), intent(in) :: date
        integer :: iD, iY, iM
        character(4) :: cY
        character(2) :: cM, cD

        cY = date(1:4)
        cM = date(5:6)
        cD = date(7:8)

        read(cY,'(I4)') iY
        read(cM,'(I2)') iM
        read(cD,'(I2)') iD

        iD = iD + 1

        if (iM == 2 .and. mod(iY, 4) == 0 .and. iD > 29) then
            cM = '03'
            cD = '01'
        else if (iM == 2 .and. mod(iY, 4) /= 0 .and. iD > 28) then
            cM = '03'
            cD = '01'
        else if ((iM == 4 .or. iM == 6 .or. iM == 9 .or. &
                  iM == 11) .and. iD > 30) then
            iM = iM + 1
            write(cM,'(I2.2)') iM
            cD = '01'
        else if (iM == 12 .and. iD > 31) then
            iY = iY + 1
            write(cY,'(I4)') iY
            cM = '01'
            cD = '01'
        else if (iD > 31) then
            iM = iM + 1
            write(cM,'(I2.2)') iM
            cD = '01'
        else
            write(cD,'(I2.2)') iD
        end if

        nextDate = cY // cM // cD

        return
    end function nextDate

    !Returns previous date as a string
    character(8) function prevDate(date)
        character(8), intent(in) :: date
        integer :: iD, iY, iM
        character(4) :: cY
        character(2) :: cM, cD

        cY = date(1:4)
        cM = date(5:6)
        cD = date(7:8)

        read(cY, '(I4)') iY
        read(cM, '(I2)') iM
        read(cD, '(I2)') iD

        iD = iD - 1

        if (iM == 3 .and. mod(iY, 4) == 0 .and. iD < 1) then
            cM = '02'
            cD = '29'
        else if (iM == 3 .and. mod(iY, 4) /= 0 .and. iD < 1) then
            cM = '02'
            cD = '28'
        else if ((iM == 5 .or. iM == 7 .or. iM == 10 .or. &
                  iM == 12) .and. iD < 1) then
            iM = iM - 1
            write(cM,'(I2.2)') iM
            cD = '30'
        else if (iM == 1 .and. iD < 1) then
            iY = iY - 1
            write(cY,'(I4)') iY
            cM = '12'
            cD = '31'
        else if (iD < 1) then
            iM = iM - 1
            write(cM,'(I2.2)') iM
            cD = '31'
        else
            write(cD,'(I2.2)') iD
        end if

        prevDate = cY // cM // cD

        return

    end function prevDate

    !If file exists, ask user if overwrite file
    !Returns true if file should be overwritten
    logical function overwriteReport(fileName)
        character(100), intent(in) :: fileName
        character :: answerYN
        logical :: fileExist, overwrite

        overwrite = .false.

        inquire(file=fileName, exist=fileExist)
        if (fileExist) then
            do
                write(*,'(x,a,a,a)') "File: ", trim(fileName), " exists."
                write(*,'(x,a)') "Do you want to overwrite file? Y/N"
                read(*,'(1a)') answerYN
                if (answerYN == 'Y' .or. answerYN == 'y') then
                    overwrite = .true.
                    exit
                else if (answerYN == 'N' .or. answerYN == 'n') then
                    overwrite = .false.
                    exit
                end if
            end do
        end if

        overwriteReport = overwrite

    end function overwriteReport

end module report
