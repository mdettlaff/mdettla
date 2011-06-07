using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Web;

namespace nReddit.Models
{
    public class NRedditEntities : DbContext
    {
        public DbSet<Submission> Submissions { get; set; }
    }
}