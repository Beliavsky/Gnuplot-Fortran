# Gnuplot-Fortran
Use subroutines to call gnuplot to make plots from Fortran in a single line of code, for example

```fortran
program test_plot
use kind_mod, only: dp
use plot_mod, only: plot, use_windows
implicit none
integer, parameter :: n = 200
real(kind=dp) :: x(n), ymat(n, 2)
integer :: i

! Set platform toggle for Windows (change to .false. for Linux/Mac)
use_windows = .true.

do i = 1, n
  x(i) = i * 0.05_dp
end do
ymat(:, 1) = sin(x)
ymat(:, 2) = cos(x)

! 1D line plot (default style)
call plot(x, ymat(:,1), title="sine")

! 1D points plot with custom labels
call plot(x, ymat(:,1), title="Sine Wave", xlabel="Time (s)", &
   ylabel="Amplitude", style="points")

! 2D default (two series vs. x)
call plot(x, ymat)

! 2D linespoints with custom title and labels
call plot(x, ymat, title="Trig Functions", xlabel="Angle (rad)", &
   ylabel="Value", style="linespoints")

end program test_plot
```
