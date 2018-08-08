import Vue from 'vue'
import Router from 'vue-router'
import Recharge from '@/components/recharge'
import Order from '@/components/recharge/order'
import Discover from '@/components/discover'
import Test from '@/components/test'
import Rank from '@/components/rank'
import RankD from '@/components/rank/rank'
import Game from '@/components/game'
import Home from '@/components/home/home'
import Room from '@/components/rooms/room'
import Room2 from '@/components/rooms'
import Person from '@/components/person/person'
import Proto from '@/components/person/proto'
import Feedback from '@/components/person/feedback'
import Login from '@/components/auth/login'
import Register from '@/components/auth/register'
Vue.use(Router)

export default new Router({
  // mode: 'history',
  routes: [ {
    path: '/game/:id?',
    name: 'game',
    component: Game,
  }, {
    path: '/person',
    name: 'person',
    component: Person
  }, {
    path: '/login',
    name: 'login',
    component: Login
  },
    {
      path: '/room/:id?',
      name: 'room',
      component: Room
    },
    {
      path: '/room2/:id?',
      name: 'room2',
      component: Room2
    },
    {
      path: '/register',
      name: 'register',
      component: Register,

    },
    {
      path: '/recharge',
      name: 'recharge',
      component: Recharge,

    },
    {
      path: '/recharge/:id',
      name: 'order',
      component: Order,

    },
    {
      path: '/proto',
      name: 'proto',
      component: Proto,
    },
    {
      path: '/feedback',
      name: 'feedback',
      component: Feedback,

    },
    {
      path: '/discover',
      name: 'discover',
      component: Discover,

    },  {
      path: '/rank',
      name: 'rank',
      component: Rank,

    },  {
      path: '/rankd/:id',
      name: 'rankd',
      component: RankD,

    },
    {
      path: '/test',
      name: 'test',
      component: Test,
    },
    {
      path: '*',
      redirect:'/room2/:id?'
    }
  ]
})
