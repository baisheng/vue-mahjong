<template>

  <div class="emoji">

    <div class="move_img" ref="movi" v-show="emojiImg">
      <img :src="emojiImg" alt="">
    </div>

    <div class="user_item u1" ref="u1">
    </div>
    <div class="user_item u2" ref="u2">
    </div>
    <div class="user_item u3" ref="u3">
    </div>
    <div class="user_item u4" ref="u4">
    </div>
  </div>
</template>


<script>
  import brg from '../../../utils/middle'
  import {mapGetters} from 'vuex'
  import Vue from 'vue'
  import $ from 'jquery'
  const TWEEN = require('tween.js');


  export default {
    data() {
      return {
        timer: null,
        emojiImg: '',
        list: [
          {
            img: require('../../../assets/icon/emoji/4/2.gif'),
            fromId: ''

          }, {
            img: '',
            fromId: ''

          }, {
            img: '',
            fromId: ''

          }, {
            img: '',
            fromId: ''

          },
        ]
      }
    },
    mounted() {


      requestAnimationFrame(animate);

      function animate(time) {
        requestAnimationFrame(animate);
        TWEEN.update(time);
      }


      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;

        if (id == 509) {
          console.log(msg)

          this.showEmoji(msg)
        }
      })


    },
    created() {

    },
    methods: {
      showEmoji(msg) {
        if (msg.type == 1) {

          this.list.forEach(v => {
            v.img = ''
          })

          let id = msg.toid;
          let user = this.members.find(v => v.PlayerId === this.user_info.id)
          let other = this.members.find(v => v.PlayerId === id)


          console.log('user postion ' + user.position)
          console.log('other postion ' + other.position)

          let imgId = parseInt(msg.content) + 1
          let img = require('../../../assets/icon/emoji/4/' + imgId + '.gif')

          let fromSite = null


          if (user.position - other.position == 1 || user.position - other.position == -3) {
            fromSite = this.$refs.u4;
            console.log('上家')
          } else if (other.position - user.position == 1 || other.position - user.position == -3) {
            fromSite = this.$refs.u2;
            console.log('下家')
          } else if (Math.abs(other.position - user.position) == 2) {
            fromSite = this.$refs.u3;
            console.log('对家')
          } else if (other.position == user.position) {
            fromSite = this.$refs.u1;
            console.log('自家')


          }

          this.$nextTick(() => {
            this.emojiImg = img
          })
          let from = {
            top: fromSite.offsetTop,
            left: fromSite.offsetLeft
          }
          $('.move_img').css({
            top: from.top,
            left: from.left,
            display: 'block'
          })

          this.timer && clearTimeout(this.timer)
          this.timer = setTimeout(() => {
            $('.move_img').css({
              top: from.top,
              left: from.left,
              display: 'none'
            })
            this.$nextTick(() => {
              this.emojiImg = ''

            })
          }, 3000)


        }
        else if (msg.type == 2) {


          let toid = msg.toid;
          let fromid = msg.fromid;


          let user = this.members.find(v => v.PlayerId === this.user_info.id)


          let other = this.members.find(v => v.PlayerId == fromid)
          let other2 = this.members.find(v => v.PlayerId == toid)


          let imgId = parseInt(msg.content)
          let img = require('../../../assets/icon/emoji/5/' + imgId + '.gif')


          let fromSite = null;
          let toSite = null

          //从哪里来
          if (user.position - other.position == 1 || user.position - other.position == -3) {

            fromSite = this.$refs.u4;
            console.log('上家')
          } else if (other.position - user.position == 1 || other.position - user.position == -3) {
            console.log('下家')
            fromSite = this.$refs.u2;
          } else if (Math.abs(other.position - user.position) == 2) {
            console.log('对家')
            fromSite = this.$refs.u3;
          } else if (other.position == user.position) {
            fromSite = this.$refs.u1
            console.log('自家')
          }


          //去哪里去
          if (user.position - other2.position == 1 || user.position - other2.position == -3) {
            toSite = this.$refs.u4;
            console.log('上家')
          } else if (other2.position - user.position == 1 || other2.position - user.position == -3) {
            toSite = this.$refs.u2;
            console.log('下家')
          } else if (Math.abs(other2.position - user.position) == 2) {
            toSite = this.$refs.u3;
            console.log('对家')
          } else if (other2.position == user.position) {
            toSite = this.$refs.u1;
            console.log('自家')
          }

          this.$nextTick(() => {
            this.emojiImg = img
            this.doAnimate(toSite, fromSite)
          })


        }
      },
      /**
       *
       * @param to
       * @param from
       */
      doAnimate(obj1, obj2){

        let from = {
          top: obj2.offsetTop,
          left: obj2.offsetLeft
        }

        let to = {
          top: obj1.offsetTop,
          left: obj1.offsetLeft
        }


        $('.move_img').css({
          top: from.top,
          left: from.left,
          display: 'block'
        })

        new TWEEN.Tween(from)
          .to(to, 600)
          .onUpdate(function () {
            $('.move_img').css({
              top: this.top,
              left: this.left
            })
          })
          .start()
          .onComplete(function () {
            setTimeout(() => {
              $('.move_img').css({
                top: 0,
                left: 0,
                display: 'none'
              })
            }, 2500)
          })

      }
    },
    computed: {
      ...mapGetters({
        'members': 'members',
        'user_info': 'user_info',
      }),

    }
  }
</script>

<style scoped lang="scss">

  .move_img {
    position: fixed;
    top: 0;
    left: 0;
    width: 1.2rem;
    height: 1.2rem;
    -webkit-transform: translate3d(0, 0, 0);
    -moz-transform: translate3d(0, 0, 0);
    -ms-transform: translate3d(0, 0, 0);
    transform: translate3d(0, 0, 0);
    -webkit-backface-visibility: hidden;
    -moz-backface-visibility: hidden;
    -ms-backface-visibility: hidden;
    backface-visibility: hidden;
    -webkit-perspective: 1000;
    -moz-perspective: 1000;
    -ms-perspective: 1000;
    perspective: 1000;

    img {
      width: 100%;
      -webkit-transform: rotate(-90deg);
      -moz-transform: rotate(-90deg);
      -ms-transform: rotate(-90deg);
      -o-transform: rotate(-90deg);
      transform: rotate(-90deg);
    }
  }

  .emoji {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    pointer-events: none;
    z-index: 999999999;
    .user_item {

      width: .8rem;
      height: .8rem;

      color: #23b586;
      position: absolute;
      -webkit-transform: rotate(-90deg);
      -moz-transform: rotate(-90deg);
      -ms-transform: rotate(-90deg);
      -o-transform: rotate(-90deg);
      transform: rotate(-90deg);
      img {
        position: absolute;
        top: 50%;
        left: 50%;
        -webkit-transform: translate(-50%, -50%);
        -moz-transform: translate(-50%, -50%);
        -ms-transform: translate(-50%, -50%);
        -o-transform: translate(-50%, -50%);
        transform: translate(-50%, -50%);
        width: 180%;
      }
      &.u4 {
        top: 91%;
        left: 40%;

      }
      &.u2 {
        top: 2%;
        left: 22%;
      }
      &.u1 {
        top: 89.5%;
        left: 72%;
      }
      &.u3 {
        top: 78%;
        left: 10%;
      }
    }
  }
</style>
