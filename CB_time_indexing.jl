#This file contains auxilary function for logical time and ad indexing

#Retrieve date and time
function get_time(i::Integer)
    temp = mod(i-1, 1440)

    hour, min = divrem(temp, 60)

    sec = 0

    return Time(hour, min, sec)
end

function get_date(i::Integer)
    duration = 1440 * [31, 28, 31, 30, 31, 30]
    borders = [1, 1+duration[1], 1+sum(duration[1:2]), 1+sum(duration[1:3]), 1+sum(duration[1:4]), 1+sum(duration[1:5])]
    ind = length(borders[borders.<=i])

    year = 2019
    month = ind
    day = 1 + div(i - borders[ind], 1440)

    return Date(year, month, day)
end

function get_datetime(i::Integer)
    return DateTime(get_date(i), get_time(i))
end


#Generate date and time
function genr_datetime()
    temp = Array{DateTime,1}(undef, 260640)

    for i = 1:260640
        temp[i] = get_datetime(i)
    end

    return temp
end

function genr_date()
    return convert(Array{Date, 1}, genr_datetime())
end

function genr_time()
    return convert(Array{Time, 1}, genr_datetime())
end


#Forwards in time
function nextMin(i::Integer, rep::Integer = 1)
    return i+rep*1
end

function nextHour(i::Integer, rep::Integer = 1)
    return i+rep*60
end

function nextDay(i::Integer, rep::Integer = 1)
    return i+rep*1440
end

function nextWeek(i::Integer, rep::Integer = 1)
    return i+rep*10080
end

function nextMonth(i::Integer, rep::Integer = 1)
    duration = 1440 * [31, 28, 31, 30, 31, 30]
    borders = [1, 1+duration[1], 1+sum(duration[1:2]), 1+sum(duration[1:3]), 1+sum(duration[1:4]), 1+sum(duration[1:5])]
    ind = length(borders[borders.<=i])


    return i+sum(duration[ind:ind+rep-1])
end

function nextCustom(i::Integer, rep::AbstractArray = [1, 1, 1, 1, 1])
    return nextMonth(i, rep[1]) + nextWeek(i, rep[2]) + nextDay(i, rep[3]) + nextHour(i, rep[4]) + nextMin(i, rep[5]) - 4*i
end


#Backwards in time
function prevMin(i::Integer, rep::Integer = 1)
    return i-rep*1
end

function prevHour(i::Integer, rep::Integer = 1)
    return i-rep*60
end

function prevDay(i::Integer, rep::Integer = 1)
    return i-rep*1440
end

function prevWeek(i::Integer, rep::Integer = 1)
    return i-rep*10080
end

function prevMonth(i::Integer, rep::Integer = 1)
    duration = 1440 * [31, 28, 31, 30, 31, 30]
    borders = [1, 1+duration[1], 1+sum(duration[1:2]), 1+sum(duration[1:3]), 1+sum(duration[1:4]), 1+sum(duration[1:5])]
    ind = length(borders[borders.<=i])

    #duration = reverse(duration)
    return i-sum(duration[ind-rep:ind - 1])
end

function prevCustom(i::Integer, rep::AbstractArray = [1, 1, 1, 1, 1])
    return prevMonth(i, rep[1]) + prevWeek(i, rep[2]) + prevDay(i, rep[3]) + prevHour(i, rep[4]) + prevMin(i, rep[5]) - 4*i
end

#Retrieve ad and datetime indices

function get_ad(data::AbstractVector, dt::DateTime)
    res = findall(x -> x==dt, data)

    return res
end

function get_ind_dt(dt::DateTime)
    temp = genr_datetime()
    return findnext(x->x==dt, temp, 1)
end
