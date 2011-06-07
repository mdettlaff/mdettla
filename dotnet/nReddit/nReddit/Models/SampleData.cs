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
            var submissions = new List<Submission> {  
                  new Submission {
                    Url = "http://www.stackoverflow.com",
                    Title = "Stack Overflow",
                    Text = "Odpowiedzi na programistyczne pytania"
                },
                new Submission {
                    Url = "http://joemonster.org",
                    Title = "Joe Monster",
                    Text = "Strona satyryczno-humorystyczna"
                }
            };
            submissions.ForEach(d => entities.Submissions.Add(d));
        }
    }

}