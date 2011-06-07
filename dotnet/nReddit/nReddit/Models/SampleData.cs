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
                    Text = "Odpowiedzi na programistyczne pytania"
                },
                new Submission {
                    Url = "http://www.msdn.microsoft.com",
                    Title = "Microsoft Developer Network",
                    Text = "Strona dla programistów"
                },
                new Submission {
                    Url = "http://slashdot.org",
                    Title = "Slashdot",
                    Text = "Wiadomości dla geeków"
                }
            };

            Subreddit funny = new Subreddit { Name = "Humor" };
            funny.Submissions = new List<Submission> {
                new Submission {
                    Url = "http://joemonster.org",
                    Title = "Joe Monster",
                    Text = "Strona satyryczno-humorystyczna"
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