module m_string_helper
    implicit none
    private
    public try_chop_prefix, try_parse_int
contains
    subroutine try_chop_prefix(prefix, string, err)
        implicit none
        character(len = *), intent(in)           :: prefix
        character(:), allocatable, intent(inout) :: string
        integer, intent(out)                     :: err
        if (len(string) < len(prefix)) then
            err = 1
        else if (string(1:len(prefix)) == prefix) then
            err = 0
            string = string(len(prefix) + 1:)
        else
            err = 1
        end if
    end subroutine try_chop_prefix

    integer function try_parse_int(string, err) result(ret)
        implicit none
        character(:), allocatable, intent(inout)  :: string
        integer, intent(out)                      :: err
        
        integer                                   :: idx, ascii

        ret = 0
        do idx = 1, len_trim(string)
            ascii = iachar(string(idx:idx))
            if ((ascii >= iachar('0')) .and. (ascii <= ichar('9'))) then
                ret = (ret * 10) + (ascii - iachar('0'))
            else
                go to 100
            end if
        end do
    100 continue
        if (idx > 1) then
            string = string(idx:)
            err = 0
        else
            err = 1
        end if
    end function try_parse_int

end module m_string_helper

module m_instruction
    use m_string_helper, only: try_chop_prefix, try_parse_int
    implicit none
    private
    public t_coord, t_box, t_instruction, try_parse_instruction, turn_on, turn_off, toggle

    type :: t_coord
        integer :: x
        integer :: y
    end type t_coord

    type :: t_box
        type(t_coord) :: top_left
        type(t_coord) :: bottom_right
    end type t_box

    enum, bind(c)
        enumerator :: instruction_types = 0
        enumerator :: turn_on           = 1
        enumerator :: turn_off          = 2
        enumerator :: toggle            = 3
    end enum

    type :: t_instruction
        type(t_box)                      :: box
        integer(kind(instruction_types)) :: instruction_type
    end type t_instruction

contains
    integer(kind(instruction_types)) function try_parse_instruction_type(line, err) result(ret)
        implicit none
        character(:), allocatable, intent(inout) :: line
        integer, intent(out)                     :: err

        call try_chop_prefix("turn on ", line, err)
        if (err == 0) then
            ret = turn_on
            return
        end if

        call try_chop_prefix("turn off ", line, err)
        if (err == 0) then
            ret = turn_off
            return
        end if

        call try_chop_prefix("toggle ", line, err)
        if (err == 0) then
            ret = toggle
            return
        end if

        err = 1
    end function try_parse_instruction_type

    type(t_coord) function try_parse_coord(string, err) result(ret)
        implicit none
        character(:), allocatable, intent(inout) :: string
        integer, intent(out)                     :: err

        ret % x = try_parse_int(string, err)
        if (err /= 0) return

        call try_chop_prefix(",", string, err)
        if (err /= 0) return

        ret % y = try_parse_int(string, err)
        if (err /= 0) return

    end function try_parse_coord

    type(t_box) function try_parse_box(string, err) result(ret)
        implicit none
        character(:), allocatable, intent(inout) :: string
        integer, intent(out)                     :: err

        ret % top_left = try_parse_coord(string, err)
        if (err /= 0) return

        call try_chop_prefix(" through ", string, err)
        if (err /= 0) return

        ret % bottom_right = try_parse_coord(string, err)
        if (err /= 0) return

    end function try_parse_box

    type(t_instruction) function try_parse_instruction(line, err) result(ret)
        implicit none
        character(:), allocatable, intent(inout) :: line
        integer, intent(out)                     :: err
        
        ret % instruction_type = try_parse_instruction_type(line, err)
        if (err /= 0) return
        
        ret % box = try_parse_box(line, err)
        if (err /= 0) return

        err = 0
    end function try_parse_instruction

end module m_instruction

subroutine print_help()
    implicit none
    write (*, "(A)") "Usage-"
    write (*, "(A)") " ./main.exe {FILEPATH}"
end subroutine print_help

program main
    use m_instruction, only: t_instruction, try_parse_instruction, turn_on, turn_off, toggle
    implicit none
    integer                                  :: num_args
    character(64)                            :: filepath
    integer                                  :: read_unit = 1
    integer                                  :: iostat, err
    type(t_instruction)                      :: ins
    character(256)                           :: iomsg
    character(256)                           :: buffer
    character(:), allocatable                :: line
    integer                                  :: lx, ux, ly, uy
    integer                                  :: grid_part_1(1000, 1000), part_1_ans
    integer                                  :: grid_part_2(1000, 1000), part_2_ans
    
    num_args = command_argument_count()

    if (num_args == 0) then
        call print_help()
        call exit(1)
        return
    end if

    call get_command_argument(1, filepath)

    open(file=filepath, unit=read_unit, iostat=iostat, iomsg=iomsg, status="old", action="read")
    if (iostat /= 0) then
        write (*, "(A, A, A, I1, A)") "Open ", trim(filepath), " failed with iostat = ", iostat, ", iomsg = " // trim(iomsg)
        call exit(1)
        return
    end if
    grid_part_1(:,:) = 0
    part_1_ans       = 0
    grid_part_2(:,:) = 0
    part_2_ans       = 0
    do
        read(read_unit, "(A)", iostat=iostat) buffer
        if (iostat /= 0) exit
        line = buffer
        ins = try_parse_instruction(line, err)
        if (err /= 0) then
            close(read_unit)
            write (*, "(A, A, A)") "Could not parse line '", trim(buffer), "', exiting!"
            call exit(1)
            return
        end if
        lx = ins % box % top_left     % x + 1
        ux = ins % box % bottom_right % x + 1
        ly = ins % box % top_left     % y + 1
        uy = ins % box % bottom_right % y + 1
        if (ins % instruction_type == turn_on) then
            grid_part_1(lx:ux, ly:uy) = 1
            grid_part_2(lx:ux, ly:uy) = grid_part_2(lx:ux, ly:uy) + 1
        else if (ins % instruction_type == turn_off) then
            grid_part_1(lx:ux, ly:uy) = 0
            grid_part_2(lx:ux, ly:uy) = max(grid_part_2(lx:ux, ly:uy) - 1, 0)
        else
            grid_part_1(lx:ux, ly:uy) = modulo(grid_part_1(lx:ux, ly:uy) + 1, 2)
            grid_part_2(lx:ux, ly:uy) = grid_part_2(lx:ux, ly:uy) + 2
        end if
    end do
    close(read_unit)

    part_1_ans = sum(grid_part_1)
    part_2_ans = sum(grid_part_2)

    write (*, *) "Part 1: ", part_1_ans
    write (*, *) "Part 2: ", part_2_ans

end program main