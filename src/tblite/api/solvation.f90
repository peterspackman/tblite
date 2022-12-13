! This file is part of tblite.
! SPDX-Identifier: LGPL-3.0-or-later
!
! tblite is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! tblite is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with tblite.  If not, see <https://www.gnu.org/licenses/>.

!> @file tblite/api/structure.f90
!> Provides API exports for the #tblite_structure handle.

!> API export for working with molecular structure data objects

module tblite_api_solvation
    use, intrinsic :: iso_c_binding
    use tblite_solvation, only : solvation_input
    use tblite_api_error, only : vp_error
    use tblite_api_version, only : namespace
    implicit none
    private
 
 
    !> Void pointer to molecular structure data
    type :: vp_solvation_input
       !> Actual payload
       type(solvation_input) :: ptr
    end type vp_solvation_input
 
 
    logical, parameter :: debug = .false.
 
 contains

end module tblite_api_solvation