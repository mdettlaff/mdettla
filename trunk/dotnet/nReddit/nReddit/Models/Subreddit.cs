using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Collections;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel;

namespace nReddit.Models
{
    public class Subreddit
    {
        public Subreddit()
        {
            Submissions = new List<Submission>();
        }

        public int SubredditID { get; set; }
        [Required(ErrorMessage = "Nazwa nie może być pusta")]
        [StringLength(32)]
        [DisplayName("Nazwa")]
        public string Name { get; set; }

        public ICollection<Submission> Submissions { get; set; }
        public int SubmissionCount { get { return Submissions.Count; } }
    }
}