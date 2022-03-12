! Thus program simulates a single-player game of chutes and ladders.

		program CLSim
		
			implicit none

			integer, parameter :: MAXGAMES = 1000, MAXTURNS = 1000, WIN = 100

			type game_record
				integer                               :: turns
				logical                               :: completed
				integer, dimension(MAXTURNS)          :: spin, landing
				character(len=6), dimension(MAXTURNS) :: event
				integer, dimension(0:MAXTURNS)        :: final
			end type game_record
			
			character(len=3)                        :: dummy
			integer, dimension(1:WIN)               :: destination
			integer                                 :: i, compgames = 0, minturns
			real                                    :: avgturns = 0, firstwins = 0
			type (game_record), dimension(MAXGAMES) :: gamenumber
			
			! Define the board
			
			do i = 1, WIN
				destination(i) = i
			end do
			
			destination(1) = 38
			destination(4) = 14
			destination(9) = 31
			destination(16) = 6
			destination(21) = 42
			destination(28) = 84
			destination(36) = 44
			destination(48) = 26
			destination(49) = 11
			destination(51) = 67
			destination(56) = 53
			destination(62) = 19
			destination(64) = 60
			destination(71) = 91
			destination(80) = 100
			destination(87) = 24
			destination(93) = 73
			destination(95) = 75
			destination(98) = 78
			
			! Use the system clock to reset the random number generator.
			! (If this step is omitted, the program will produce the
			! same sequence of games every time it is run.)

			call init_random_seed()
			
			! Simulate many games.
			
			print*,'Simulating ', MAXGAMES, 'games.'
			do i = 1, MAXGAMES
				call simgame(gamenumber(i))
			end do
			
			! Compute average number of complete games and average
			! number of turns per complete game.
			
			do i = 1, MAXGAMES
				if (gamenumber(i)%completed) then
					compgames = compgames + 1
					avgturns = avgturns + gamenumber(i)%turns
				end if
			end do
			avgturns = avgturns/compgames
			print*,'SINGLE-PLAYER GAMES'
			print*,'There were', compgames, 'complete games out of', MAXGAMES, '.'
			print*,'The average number of turns per complete game was', avgturns, '.'
			print*,' '
			
			! Compute the average number of turns per complete game
			! between two players.
			
			compgames = 0
			avgturns = 0
			do i = 1, MAXGAMES, 2
				if (i+1 .gt. MAXGAMES) exit
				if (gamenumber(i)%completed .or. gamenumber(i+1)%completed) then
					compgames = compgames + 1
					minturns =  min(gamenumber(i)%turns,gamenumber(i+1)%turns)
					avgturns = avgturns + minturns
					if (gamenumber(i)%turns .eq. minturns) firstwins = firstwins + 1
				end if
			end do
			avgturns = avgturns/compgames
			firstwins = firstwins/compgames
			print*,'TWO-PLAYER GAMES'
			print*,'There were', compgames, 'complete games out of', MAXGAMES/2, '.'
			print*,'The average number of turns per complete game was', avgturns, '.'
			print*,'The first player won', 100*firstwins, 'percent of the time.'
			print*,' '
			
			! Compute the average number of turns per complete game
			! between three players.
			
			compgames = 0
			avgturns = 0
			firstwins = 0
			do i = 1, MAXGAMES, 3
				if (i+2 .gt. MAXGAMES) exit
				if (gamenumber(i)%completed .or. gamenumber(i+1)%completed .or. gamenumber(i+2)%completed) then
					compgames = compgames + 1
					minturns = min(gamenumber(i)%turns,gamenumber(i+1)%turns,gamenumber(i+2)%turns)
					avgturns = avgturns + minturns
					if (gamenumber(i)%turns .eq. minturns) firstwins = firstwins + 1
				end if
			end do
			avgturns = avgturns/compgames
			firstwins = firstwins/compgames
			print*,'THREE-PLAYER GAMES'
			print*,'There were', compgames, 'complete games out of 333.'
			print*,'The average number of turns per complete game was', avgturns, '.'
			print*,'The first player won', 100*firstwins, 'percent of the time.'
			print*,' '
			
			contains
				
				subroutine init_random_seed()
					integer :: i, n, clock
					integer, dimension(:), allocatable :: seed
					call random_seed(size=n)
					allocate(seed(n))
					call system_clock(count=clock)
					seed = clock + 37*(/ (i - 1, i = 1, n)/)
					call random_seed(put=seed)
					deallocate(seed)
				end subroutine init_random_seed
			
				subroutine spin(k)
					integer, intent(out) :: k
					real                 :: x
					call random_number(x)
					k = ceiling(6*x)
					return
				end subroutine spin

				subroutine simgame(x)
					
					logical, parameter :: DEBUG = .false.
					
					integer                        :: i = 0
					type(game_record), intent(out) :: x
					
					! Initializations
					
					x%completed = .false.
					x%final(0) = 0
					
					do i = 1, MAXTURNS
					
						! Move, if possible.  (The player cannot move if
						! his spin would take him past the end of the
						! game board.)
						
						call spin(x%spin(i))
						if (x%final(i-1) + x%spin(i) .le. WIN) then
							x%landing(i) = x%final(i-1) + x%spin(i)
						else
							x%landing(i) = x%final(i-1)
						end if
						
						! Go down chute or up ladder, if necessary
						
						x%final(i) = destination(x%landing(i))
						if (x%final(i) .gt. x%landing(i)) then
							x%event(i) = 'Ladder'
						else if (x%final(i) .eq. x%landing(i)) then
							x%event(i) = 'None'
						else
							x%event(i) = 'Chute'
						end if
						
						! Optional debug output

						if (DEBUG) print 100, i, x%spin(i), x%landing(i), x%event(i), x%final(i)
						
						! Check for victory
						
						if (x%final(i) .eq. WIN) then
							x%turns = i
							x%completed = .true.
							if (DEBUG) write(*,"('Won on turn ', I3)") x%turns
							exit
						end if
						
					end do
 
 100                format('Turn: ', I3, ' Spin: ', I3, ' Landing: ', I3, ' Event: ', A6, ' Final: ', I3)

					return
				
				end subroutine simgame
				
				subroutine showgame(x)
					
					character(len=4), parameter :: INT_FORMAT = '(i4)'
					integer, parameter          :: LINELENGTH = 72
					
					character(len=LINELENGTH)     :: screenline
					character(len=4)              :: temp
					integer                       :: i
					type(game_record), intent(in) :: x
					
					print*,'This is the record of a simulated game of chutes and ladders.'
					print*,' '
					
					do i = 1, min(x%turns, MAXTURNS)
						write(temp,INT_FORMAT) i
						screenline = 'On turn '//trim(adjustl(temp))//', the player spun '
						write(temp,INT_FORMAT) x%spin(i) 
						screenline = trim(screenline)//' '//adjustl(temp)
						write(temp,INT_FORMAT) x%landing(i)
						screenline = trim(screenline)//' and landed on space '//trim(adjustl(temp))//'.'
						print*, screenline
						write(temp,INT_FORMAT) x%final(i)
						if (x%event(i) .eq. 'Chute') then 
							screenline = 'The player fell down a chute to space '//trim(adjustl(temp))//'.'
							print*, screenline
						else if (x%event(i) .eq. 'Ladder') then
							write(temp,INT_FORMAT) x%final(i)
							screenline = 'The player climbed a ladder to space '//trim(adjustl(temp))//'.'
							print*, screenline
						end if
						if (x%final(i) .eq. WIN) then
							print*,'The player won the game!'
						end if
					end do
					
					return
				
				end subroutine showgame
		
		end program CLSim
