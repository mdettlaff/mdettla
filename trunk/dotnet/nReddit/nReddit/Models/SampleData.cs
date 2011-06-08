using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Web;

namespace nReddit.Models
{
    public class SampleData : DropCreateDatabaseIfModelChanges<NRedditEntities>
    {
        protected override void Seed(NRedditEntities entities)
        {
            Subreddit programming = new Subreddit { Name = "Programowanie" };
            programming.Submissions = new List<Submission> {  
                new Submission {
                    Url = "http://www.stackoverflow.com",
                    Title = "Stack Overflow",
                    Text = "Odpowiedzi na programistyczne pytania",
                    UpvoteCount = 55, DownvoteCount = 10,
                    Username = "mdettla", UsernamesOfPeopleWhoVoted = ""
                },
                new Submission {
                    Url = "http://www.msdn.microsoft.com",
                    Title = "Microsoft Developer Network",
                    Text = "Strona dla programistów",
                    UpvoteCount = 20, DownvoteCount = 18,
                    Username = "Administrator", UsernamesOfPeopleWhoVoted = ""
                },
                new Submission {
                    Url = "http://slashdot.org",
                    Title = "Slashdot",
                    Text = "Wiadomości dla geeków",
                    UpvoteCount = 80, DownvoteCount = 15,
                    Username = "mdettla", UsernamesOfPeopleWhoVoted = ""
                }
            };

            Subreddit funny = new Subreddit { Name = "Humor" };
            funny.Submissions = new List<Submission> {
                new Submission {
                    Url = "http://joemonster.org",
                    Title = "Joe Monster",
                    Text = "Strona satyryczno-humorystyczna",
                    UpvoteCount = 30, DownvoteCount = 11,
                    Username = "mdettla", UsernamesOfPeopleWhoVoted = ""
                }
            };

            var subreddits = new List<Subreddit>
            {
                programming, funny
            };
            subreddits.ForEach(d => entities.Subreddits.Add(d));
            entities.SaveChanges();
        }
    }
}