use praxis

select * from matches
select * from balls

--Number of matches played in different stadiums
select count(*) as number_of_matches,venue from matches group by venue order by number_of_matches desc

--Number of matches played in different city
select count(*) as number_of_matches,city from matches group by city order by number_of_matches desc

--Number of matches played by each team
select team1 as Team,SUM(Total) as Total_Matches_Played
from
(select team1,count(team1) as Total from matches group by team1
union all
select team2,count(team2) as Total from matches group by team2
) matches group by team1 order by Total_Matches_Played desc

--Number of matches won by each team
select winner,count(winner) as TotalWon from matches group by winner order by TotalWon desc

--Number of toss won by each team
select toss_winner,count(toss_winner) as TotalTossWon from matches group by toss_winner order by TotalTossWon desc

--Top 10 wicket takers
select top 10 bowler,sum(is_wicket) as TotalWickets from balls group by bowler order by TotalWickets desc

--Top 10 run scores
select top 10 batsman,sum(batsman_runs) as TotalRuns from balls group by batsman order by TotalRuns desc

--Total runs in each delivery
select ball,sum(total_runs) as TotalRuns from balls where ball not in (7,8,9) group by ball order by TotalRuns desc

--Average runs in each delivery
select ball,round(avg(cast(total_runs as Float)),4) as AverageRuns from balls where ball not in (7,8,9) group by ball order by AverageRuns desc

--Average runs in each over
select [over],round(avg(cast(total_runs as Float)*6),2) as AverageRuns from balls group by [over] order by AverageRuns desc

--Total number of runs scored in each over
select [over],sum(total_runs) as TotalRuns from balls group by [over] order by TotalRuns desc

--Total number of wickets in each over
select [over],sum(is_wicket) as TotalWickets from balls group by [over] order by TotalWickets desc

--Number of matches won by team who lost the toss
select count(*)-4 from matches where toss_winner <> winner --4 matches had no results

--Number of matches won by team who won the toss
select count(*) from matches where toss_winner = winner 

--Number of matches won by team after winning the toss
select toss_winner,count(*) as TotalTossWon from matches where toss_winner = winner group by toss_winner order by TotalTossWon desc

--Number of matches won by team after loosing the toss
select winner,count(*) as TotalTossLost from matches where toss_winner <> winner group by winner order by TotalTossLost desc

--Number of matches won by home team
select team1,count(*) as TotalHomeWon from matches where team1 = winner  group by team1 order by TotalHomeWon desc

--Number of matches won by away team
select team2,count(*) as TotalAwayWon from matches where team2 = winner  group by team2 order by TotalAwayWon desc

--Average runs by team batting first
select batting_team,round(avg(cast(total_runs as float))*120,2) as AverageScore from balls where inning=1 group by batting_team order by AverageScore desc

--Average runs by team batting second
select batting_team,round(avg(cast(total_runs as float))*120,2) as AverageScore from balls where inning=2 group by batting_team order by AverageScore desc

--Number of balls bowled by bowlers
select top 10 bowler,count(*) as TotalBallsBowled from balls group by bowler order by TotalBallsBowled desc

--Number of runs conceded by bowlers
select top 10 bowler,sum(total_runs) as TotalRunsConceded from balls group by bowler order by TotalRunsConceded desc

--Most 6's hit by a batsman
select top 10 batsman,count(*) as TotalSixHit from balls where batsman_runs=6 group by batsman order by TotalSixHit desc

--Most 6's conceded by a bowler
select top 10 bowler,count(*) as TotalSixConceded from balls where batsman_runs=6 group by bowler order by TotalSixConceded desc

--Most number of catches taken
select top 10 fielder,count(*) as TotalCatchesTaken from balls where dismissal_kind='caught' group by fielder order by TotalCatchesTaken desc

--Each innings score by teams
select matches.id, [date] ,inning,batting_team,sum(total_runs) as RunsScored from balls inner join matches on balls.id = matches.id group by matches.id,[date],inning,batting_team order by matches.id,inning,[date] asc